"""Focused RETAINED-1 CELL_RECT32 and physical-clip contract checks."""

from __future__ import annotations

import struct
from pathlib import Path

import pytest

from rich_terminal.apt1 import UINT32_MAX
from rich_terminal.pygame_view import composite_draw_plane
from rich_terminal.retained_scene import (
    ControlKind,
    ControlState,
    GroupBody,
    ObjectBounds,
    OwnerIdentity,
    Point,
    RGBA,
    RegionDefinition,
    Sample,
    SceneErrorCode,
    SceneModelError,
)
from rich_terminal.retained_view import (
    GlyphRunDraw,
    MenuBarDraw,
    MenuDraw,
    PlotDraw,
    PolylineDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    SeriesHistoryDraw,
    StatusDraw,
)
from rich_terminal.retained_wire import (
    ControlWireDefinition,
    ObjectWireDefinition,
    RegionWireDefinition,
    RetainedFormats,
    decode_control_definition,
    decode_object_definition,
    decode_region_definition,
    decode_ret_formats,
    encode_control_definition,
    encode_object_definition,
    encode_region_definition,
    encode_ret_formats,
)
from rich_terminal.update_authority import TerminalGeometry


INT32_MIN = -(1 << 31)
INT32_MAX = (1 << 31) - 1
ROOT = Path(__file__).resolve().parents[1]


class _PixelFont:
    def __init__(self, pygame_module):
        self._pygame = pygame_module
        self.rendered: list[str] = []

    def get_linesize(self):
        return 1

    def size(self, text):
        return len(text), 1

    def render(self, text, antialias, color):
        assert antialias and len(text) == 1
        self.rendered.append(text)
        glyph = self._pygame.Surface((1, 1), flags=self._pygame.SRCALPHA)
        glyph.fill((*color, 0xFF))
        return glyph


def test_exact_wire_layouts_use_cell_rect32_and_keep_path_points_unorm32():
    formats = RetainedFormats(2, 1, 0, 0, 0, 8, 32, 0, 0, 0, 0, 64)
    assert decode_ret_formats(encode_ret_formats(formats)) == formats
    assert struct.unpack_from("<I", encode_ret_formats(formats), 0) == (2,)

    region = RegionWireDefinition(
        1,
        2,
        3,
        -4,
        -5,
        UINT32_MAX,
        UINT32_MAX,
        0,
        0,
        7,
        6,
        -9,
        0x3,
    )
    region_payload = encode_region_definition(region)
    assert len(region_payload) == 64
    assert struct.unpack("<QQQiiIIIIIIiI", region_payload) == (
        1,
        2,
        3,
        -4,
        -5,
        UINT32_MAX,
        UINT32_MAX,
        0,
        0,
        7,
        6,
        -9,
        0x3,
    )
    assert decode_region_definition(region_payload) == region

    bounds = ObjectBounds(INT32_MIN, INT32_MAX, UINT32_MAX, UINT32_MAX)
    object_definition = ObjectWireDefinition(
        1,
        2,
        4,
        3,
        0,
        bounds,
        -10,
        True,
        GroupBody(),
    )
    object_payload = encode_object_definition(object_definition)
    assert len(object_payload) == 64
    assert struct.unpack_from("<iiII", object_payload, 48) == (
        INT32_MIN,
        INT32_MAX,
        UINT32_MAX,
        UINT32_MAX,
    )
    assert decode_object_definition(object_payload) == object_definition

    root = ControlWireDefinition(
        1,
        2,
        5,
        ControlKind.MENU_BAR,
        ControlState.VISIBLE | ControlState.ENABLED,
        0,
        3,
        0,
        0,
        bounds,
        "",
        "",
    )
    root_payload = encode_control_definition(root)
    assert len(root_payload) == 80
    assert struct.unpack_from("<iiII", root_payload, 52) == (
        INT32_MIN,
        INT32_MAX,
        UINT32_MAX,
        UINT32_MAX,
    )
    assert decode_control_definition(root_payload) == root

    child = ControlWireDefinition(
        1,
        2,
        6,
        ControlKind.MENU,
        ControlState.VISIBLE | ControlState.ENABLED,
        0,
        3,
        5,
        0,
        None,
        "File",
        "",
    )
    child_payload = encode_control_definition(child)
    assert len(child_payload) == 84
    assert child_payload[52:68] == bytes(16)
    assert decode_control_definition(child_payload) == child


def test_region_clip_validation_uses_wide_logical_endpoints_and_canonical_empty():
    owner = OwnerIdentity(11, 0, 1, 1)
    geometry = TerminalGeometry(8, 5, 7)
    RegionDefinition(
        owner,
        1,
        INT32_MIN,
        -2,
        UINT32_MAX,
        UINT32_MAX,
        0,
        0,
        8,
        5,
        0,
        True,
        True,
        7,
    ).validate_geometry(geometry)
    RegionDefinition(
        owner,
        2,
        INT32_MAX,
        INT32_MAX,
        UINT32_MAX,
        UINT32_MAX,
        0,
        0,
        0,
        0,
        0,
        True,
        True,
        7,
    ).validate_geometry(geometry)

    outside_intersection = RegionDefinition(
        owner,
        3,
        -2,
        0,
        3,
        2,
        0,
        0,
        2,
        2,
        0,
        True,
        True,
        7,
    )
    with pytest.raises(SceneModelError) as caught:
        outside_intersection.validate_geometry(geometry)
    assert caught.value.code is SceneErrorCode.BOUNDS

    with pytest.raises(ValueError, match="all-zero"):
        RegionDefinition(
            owner,
            4,
            0,
            0,
            1,
            1,
            1,
            0,
            0,
            0,
            0,
            True,
            True,
            7,
        )
    with pytest.raises(ValueError, match="zero clip"):
        RegionDefinition(
            owner,
            5,
            0,
            0,
            1,
            1,
            0,
            0,
            1,
            1,
            0,
            True,
            False,
            7,
        )


def test_extreme_logical_geometry_only_reaches_pygame_as_bounded_visible_work(
    monkeypatch,
):
    pygame = pytest.importorskip("pygame")
    original_surface = pygame.Surface
    destination = original_surface((4, 3))
    destination.fill((3, 4, 5))
    allocations: list[tuple[int, int]] = []

    def recording_surface(size, *args, **kwargs):
        measured = tuple(size)
        allocations.append(measured)
        assert 0 < measured[0] <= destination.get_width()
        assert 0 < measured[1] <= destination.get_height()
        return original_surface(size, *args, **kwargs)

    monkeypatch.setattr(pygame, "Surface", recording_surface)
    font = _PixelFont(pygame)
    huge = ObjectBounds(INT32_MAX, 0, UINT32_MAX, 2)
    glyph = GlyphRunDraw(
        1,
        0,
        ObjectBounds(0, 0, UINT32_MAX, 2),
        RGBA(240, 240, 240, 255),
        RGBA(40, 60, 80, 128),
        0,
        "X",
    )
    polyline = PolylineDraw(
        2,
        1,
        huge,
        (Point(0, 0), Point(UINT32_MAX, UINT32_MAX)),
        UINT32_MAX,
        RGBA(200, 20, 30, 128),
        False,
    )
    status = StatusDraw(
        3,
        2,
        ObjectBounds(INT32_MAX, 0, 3, 2),
        RGBA(10, 20, 30, 128),
        RGBA(30, 200, 80, 128),
        1,
        1,
    )
    plot = PlotDraw(
        4,
        3,
        huge,
        1,
        0,
        10,
        RGBA(100, 180, 240, 128),
        RGBA(20, 60, 100, 128),
        True,
        False,
    )
    menu = MenuBarDraw(
        10,
        ControlState.VISIBLE | ControlState.ENABLED,
        0,
        4,
        ObjectBounds(INT32_MAX - 5, 0, UINT32_MAX, 2),
        (
            MenuDraw(
                11,
                ControlState.VISIBLE | ControlState.ENABLED,
                0,
                "A",
                (),
            ),
        ),
    )
    region = RetainedRegionDraw(
        1,
        1,
        1,
        INT32_MIN,
        0,
        UINT32_MAX,
        2,
        0,
        0,
        2,
        2,
        0,
        True,
        (glyph, polyline, status, plot, menu),
    )
    plane = RetainedDrawPlane(
        True,
        True,
        (region,),
        (SeriesHistoryDraw(1, 1, 1, (Sample(0, 0), Sample(1, 10))),),
    )

    composite_draw_plane(pygame, destination, plane, font, 2, 1)

    assert allocations
    assert font.rendered == ["X"]
    assert tuple(destination.get_at((0, 0)))[:3] != (3, 4, 5)
    source = (ROOT / "rich_terminal" / "pygame_view.py").read_text(encoding="utf-8")
    allocation_lines = [line for line in source.splitlines() if "pygame_module.Surface(" in line]
    assert allocation_lines
    assert all("visible.size" in line for line in allocation_lines)


def test_menu_measurement_never_renders_one_unbounded_whole_label():
    pygame = pytest.importorskip("pygame")

    class _RenderOnlyFont:
        def render(self, *_args, **_kwargs):
            raise AssertionError("whole-label render fallback must not run")

    menu = MenuBarDraw(
        10,
        ControlState.VISIBLE | ControlState.ENABLED,
        0,
        0,
        ObjectBounds(0, 0, 2, 1),
        (
            MenuDraw(
                11,
                ControlState.VISIBLE | ControlState.ENABLED,
                0,
                "File",
                (),
            ),
        ),
    )
    region = RetainedRegionDraw(
        1,
        1,
        1,
        0,
        0,
        2,
        1,
        0,
        0,
        2,
        1,
        0,
        True,
        (menu,),
    )
    with pytest.raises(TypeError, match=r"non-rendering size\(\)"):
        composite_draw_plane(
            pygame,
            pygame.Surface((2, 1)),
            RetainedDrawPlane(True, True, (region,)),
            _RenderOnlyFont(),
            1,
            1,
        )


def test_forth_public_payload_offsets_and_cell_geometry_are_locked():
    source = (ROOT / "rich-terminal.f").read_text(encoding="utf-8")
    assert "_PT-RV-RETMAX @ 264 U<" in source
    assert "_PT-RF-FORMATS @ L@ 2 <>" in source
    assert "CREATE _PT-REGION-PAYLOAD 64 ALLOT" in source
    for offset, variable in (
        (24, "_PT-RG-LOGICAL-X"),
        (28, "_PT-RG-LOGICAL-Y"),
        (32, "_PT-RG-LOGICAL-COLS"),
        (36, "_PT-RG-LOGICAL-ROWS"),
        (40, "_PT-RG-CLIP-X"),
        (44, "_PT-RG-CLIP-Y"),
        (48, "_PT-RG-CLIP-COLS"),
        (52, "_PT-RG-CLIP-ROWS"),
        (56, "_PT-RG-Z"),
        (60, "_PT-RG-FLAGS"),
    ):
        assert f"{variable} @ _PT-REGION-PAYLOAD {offset} + L!" in source
    assert "_PT-OB-X @ _PT-FRAME-PAYLOAD 48 + L!" in source
    assert "_PT-OB-Y @ _PT-FRAME-PAYLOAD 52 + L!" in source
    assert "_PT-OB-COLS @ _PT-FRAME-PAYLOAD 56 + L!" in source
    assert "_PT-OB-ROWS @ _PT-FRAME-PAYLOAD 60 + L!" in source
    assert "_PT-CT-X @ _PT-FRAME-PAYLOAD 52 + L!" in source
    assert "_PT-CT-ROWS @ _PT-FRAME-PAYLOAD 64 + L!" in source
    assert "_PT-OB-LEFT" not in source
    assert "_PT-CT-BOTTOM" not in source
    assert "_PT-PR-XEND @ 0> 0=" in source
    assert "_PT-PR-YEND @ 0> 0=" in source
    assert "0<=" not in source
