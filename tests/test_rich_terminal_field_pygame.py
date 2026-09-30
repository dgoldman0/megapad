"""Typed fields retain guest geometry and only expose their writable value area."""

from dataclasses import replace

import pytest

pygame = pytest.importorskip("pygame")

from display import VirtualTerminal
from rich_terminal.appearance import FLOWING_APPEARANCE, REFERENCE_APPEARANCE
from rich_terminal.pygame_view import FieldHitTarget, composite_draw_plane_result
from rich_terminal.retained_scene import ControlState, ObjectBounds
from rich_terminal.retained_view import FieldDraw, RetainedDrawPlane, RetainedRegionDraw
from rich_terminal.semantic_fields import FieldChoice, FieldContent, FieldFlag, FieldKind, FieldRect
from session_viewer import compose_terminal_frame_changes, compose_terminal_frame_result

ACTIVE = ControlState.VISIBLE | ControlState.ENABLED


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _field(kind=FieldKind.INTEGER, *, flags=FieldFlag(0), state=ACTIVE, origin=(1, 2)):
    values = dict(content_revision=1, kind=kind, flags=flags,
                  label_bounds=FieldRect(0, 0, 10, 1), value_bounds=FieldRect(12, 0, 8, 1))
    if kind is FieldKind.INTEGER:
        values.update(value=440, minimum=40, maximum=2000, step=10)
    elif kind is FieldKind.CHOICE:
        values.update(value=17, choices=(FieldChoice(17, "Sine"), FieldChoice(-9, "Saw")))
    else:
        values.update(text="Voice")
    return FieldDraw(1, state, 0, 0, ObjectBounds(*origin, 20, 1), "Frequency", FieldContent(**values))


def _plane(field):
    return RetainedDrawPlane(True, True, (RetainedRegionDraw(
        1, 1, 1, 0, 0, 30, 10, 0, 0, 30, 10, 0, True,
        () if field is None else (field,),
    ),))


def _paint(field, font, appearance, surface=None):
    if surface is None:
        surface = pygame.Surface((240, 120))
        surface.fill((63, 22, 97))
    return composite_draw_plane_result(pygame, surface, _plane(field), font, 8, 12,
                                       appearance=appearance)


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
@pytest.mark.parametrize("kind", list(FieldKind))
def test_only_value_slot_is_activatable_and_numeric_choice_slots_adjust(font, appearance, kind):
    result = _paint(_field(kind), font, appearance)
    assert len(result.hit_targets) == 1
    target = result.hit_targets[0]
    assert isinstance(target, FieldHitTarget)
    assert (target.rect.left, target.rect.top, target.rect.width, target.rect.height) == (104, 24, 64, 12)
    assert target.content_revision == 1
    assert target.adjustable == (kind is not FieldKind.TEXT)
    assert result.hit_test(30, 30) is None
    assert result.hit_test(97, 30) is None
    assert result.hit_test(110, 30) == target


@pytest.mark.parametrize("flags,state", [(FieldFlag.READ_ONLY, ACTIVE), (FieldFlag(0), ControlState.VISIBLE)])
def test_readonly_and_disabled_fields_paint_without_input_target(font, flags, state):
    result = _paint(_field(flags=flags, state=state), font, FLOWING_APPEARANCE)
    assert not result.hit_targets
    assert result.hit_entries
    assert result.hit_test(110, 30) is None


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_long_labels_and_values_remain_within_their_declared_slots(font, appearance):
    field = _field(FieldKind.TEXT)
    ordinary = _paint(field, font, appearance).surface
    long_label = _paint(replace(field, label="Label " * 50), font, appearance).surface
    long_value = _paint(replace(field, content=replace(field.content, text="Value " * 50)), font, appearance).surface
    label = pygame.Rect(8, 24, 80, 12)
    value = pygame.Rect(104, 24, 64, 12)
    assert pygame.image.tobytes(ordinary.subsurface(value), "RGBA") == pygame.image.tobytes(long_label.subsurface(value), "RGBA")
    assert pygame.image.tobytes(ordinary.subsurface(label), "RGBA") == pygame.image.tobytes(long_value.subsurface(label), "RGBA")


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_field_value_revision_readonly_move_drop_partial_repaint_matches_full(font, appearance):
    terminal = VirtualTerminal(cols=30, rows=10)
    terminal.write(b"CELL fallback and underlying controls")
    previous = None
    glyph_cache = {}
    first = _field()
    for field in (first, replace(first, state=ACTIVE | ControlState.SELECTED),
                  replace(first, content=replace(first.content, value=441, content_revision=2)),
                  _field(FieldKind.CHOICE), _field(flags=FieldFlag.READ_ONLY),
                  _field(origin=(-3, 6)), None):
        incremental = compose_terminal_frame_changes(
            pygame, terminal, font, 8, 12, retained_plane=_plane(field), show_cursor=False,
            previous=previous, appearance=appearance, glyph_cache=glyph_cache,
        )
        full = compose_terminal_frame_result(
            pygame, terminal, font, 8, 12, retained_plane=_plane(field),
            show_cursor=False, appearance=appearance,
        )
        assert pygame.image.tobytes(incremental.surface, "RGBA") == pygame.image.tobytes(full.surface, "RGBA")
        assert incremental.hit_entries == full.hit_entries
        previous = incremental


def test_signed_coordinate_field_and_input_clip_stay_surface_bounded(font):
    content = FieldContent(1, FieldKind.TEXT, FieldFlag(0), FieldRect(0, 0, 0, 0),
                           FieldRect((1 << 31) - 1, 0, 8, 1), text="Edge")
    field = FieldDraw(1, ACTIVE, 0, 0, ObjectBounds(-(1 << 31), 2, (1 << 32) - 1, 1), "", content)
    result = _paint(field, font, FLOWING_APPEARANCE)
    assert result.hit_targets[0].rect.left == 0
    assert result.hit_targets[0].rect.width == 56
    assert result.surface.get_at((20, 50))[:3] == (63, 22, 97)


def test_field_does_not_expand_caller_clip(font):
    surface = pygame.Surface((240, 120))
    surface.fill((63, 22, 97))
    clip = pygame.Rect(110, 25, 7, 7)
    surface.set_clip(clip)
    _paint(_field(), font, FLOWING_APPEARANCE, surface)
    assert surface.get_clip() == clip
    assert surface.get_at((100, 30))[:3] == (63, 22, 97)
    assert surface.get_at((112, 29))[:3] != (63, 22, 97)
