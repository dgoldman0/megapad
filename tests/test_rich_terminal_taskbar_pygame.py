"""Taskbar pixels and hit regions use authored cells under either appearance."""

from dataclasses import replace

import pytest

pygame = pytest.importorskip("pygame")

from display import VirtualTerminal
from rich_terminal.appearance import FLOWING_APPEARANCE, REFERENCE_APPEARANCE
from rich_terminal.pygame_view import ControlHitTarget, composite_draw_plane_result
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_view import RetainedDrawPlane, RetainedRegionDraw, TaskBarDraw, TaskDraw
from session_viewer import compose_terminal_frame_changes, compose_terminal_frame_result

ACTIVE = ControlState.VISIBLE | ControlState.ENABLED


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _bar(*, selected=False, minimized=False, enabled=True, label="Pad", origin=(1, 8)):
    tasks = (
        TaskDraw(11, ControlKind.TASK, ACTIVE | (ControlState.SELECTED if selected else 0),
                 0, ObjectBounds(0, 0, 7, 1), label, ""),
        TaskDraw(12, ControlKind.TASK, ACTIVE | (ControlState.MINIMIZED if minimized else 0),
                 1, ObjectBounds(8, 0, 8, 1), "Sound", ""),
        TaskDraw(13, ControlKind.LAUNCHER, ACTIVE, 2, ObjectBounds(19, 0, 5, 1), "Apps", ""),
    )
    return TaskBarDraw(10, ACTIVE if enabled else ControlState.VISIBLE, 0, 0,
                       ObjectBounds(*origin, 26, 1), tasks)


def _plane(bar):
    return RetainedDrawPlane(True, True, (RetainedRegionDraw(
        1, 1, 1, 0, 0, 30, 10, 0, 0, 30, 10, 0, True,
        () if bar is None else (bar,),
    ),))


def _paint(bar, font, appearance, *, hovered=None, pressed=None):
    surface = pygame.Surface((240, 120))
    surface.fill((63, 22, 97))
    return composite_draw_plane_result(pygame, surface, _plane(bar), font, 8, 12,
                                       hovered=hovered, pressed=pressed, appearance=appearance)


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_task_hit_rectangles_ignore_label_font_and_selected_material(font, appearance):
    first = _paint(_bar(), font, appearance)
    changed = _paint(_bar(selected=True, minimized=True, label="Very long title " * 30), font, appearance)
    targets = tuple(entry for entry in first.hit_targets if isinstance(entry, ControlHitTarget))
    assert first.hit_targets == changed.hit_targets
    assert [(hit.identity.control_id, hit.rect.left, hit.rect.top, hit.rect.width, hit.rect.height)
            for hit in targets] == [(11, 8, 96, 56, 12), (12, 72, 96, 64, 12), (13, 160, 96, 40, 12)]
    larger = pygame.font.Font(None, 22)
    assert _paint(_bar(), larger, appearance).hit_targets == first.hit_targets
    assert first.hit_test(67, 100) is None
    assert first.hit_test(150, 100) is None
    assert first.hit_test(165, 100).kind is ControlKind.LAUNCHER


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_selected_and_pressed_task_material_never_changes_separators(font, appearance):
    plain = _paint(_bar(), font, appearance)
    target = plain.hit_targets[0]
    changed = _paint(_bar(selected=True), font, appearance,
                     hovered=target.identity, pressed=target.identity)
    allowed = pygame.Rect(8, 96, 56, 12)
    changed_pixels = 0
    for y in range(120):
        for x in range(240):
            if plain.surface.get_at((x, y)) != changed.surface.get_at((x, y)):
                changed_pixels += 1
                assert allowed.collidepoint(x, y)
    assert changed_pixels


def test_disabled_taskbar_blocks_lower_entries_without_activation(font):
    result = _paint(_bar(enabled=False), font, FLOWING_APPEARANCE)
    assert not result.hit_targets
    assert result.hit_test(20, 100) is None
    assert result.hit_entries


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_task_state_text_move_drop_partial_repaint_matches_full(font, appearance):
    terminal = VirtualTerminal(cols=30, rows=10)
    terminal.write(b"Underlying CELL fallback")
    previous = None
    glyph_cache = {}
    for bar in (_bar(), _bar(selected=True), _bar(minimized=True),
                _bar(label="Long title"), _bar(origin=(-2, 7)), _bar(enabled=False), None):
        incremental = compose_terminal_frame_changes(
            pygame, terminal, font, 8, 12, retained_plane=_plane(bar), show_cursor=False,
            previous=previous, appearance=appearance, glyph_cache=glyph_cache,
        )
        full = compose_terminal_frame_result(
            pygame, terminal, font, 8, 12, retained_plane=_plane(bar),
            show_cursor=False, appearance=appearance,
        )
        assert pygame.image.tobytes(incremental.surface, "RGBA") == pygame.image.tobytes(full.surface, "RGBA")
        assert incremental.hit_entries == full.hit_entries
        previous = incremental


def test_extreme_root_coordinates_clip_before_native_rectangle_creation(font):
    bar = TaskBarDraw(10, ACTIVE, 0, 0, ObjectBounds(-(1 << 31), 8, (1 << 32) - 1, 1), (
        TaskDraw(11, ControlKind.TASK, ACTIVE, 0,
                 ObjectBounds((1 << 31) - 1, 0, 8, 1), "Edge", ""),
    ))
    result = _paint(bar, font, FLOWING_APPEARANCE)
    assert result.hit_targets[0].rect.left == 0
    assert result.hit_targets[0].rect.width == 56
    assert result.surface.get_at((100, 50))[:3] == (63, 22, 97)
