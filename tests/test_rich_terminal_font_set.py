"""Viewer font sets: a fallback face per character, fitted to its cells."""

from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from rich_terminal import font_set
from rich_terminal.font_set import FontSet, discover_fallback_fonts


def test_discovery_keeps_only_the_families_fontconfig_has(monkeypatch, tmp_path):
    present = tmp_path / "Present.ttf"
    present.write_bytes(b"font")

    def run(command, **_kwargs):
        family = command[-1]
        if family == "Present":
            return subprocess.CompletedProcess(command, 0, f"{present}\nPresent", "")
        if family == "Substituted":
            return subprocess.CompletedProcess(command, 0, f"{present}\nDejaVu Sans", "")
        return subprocess.CompletedProcess(command, 1, "", "")

    monkeypatch.setattr(font_set.subprocess, "run", run)
    assert discover_fallback_fonts(
        ("Missing", "Present", "Substituted", "Present")
    ) == (present,)


def test_discovery_without_fontconfig_finds_nothing(monkeypatch):
    def run(command, **_kwargs):
        raise FileNotFoundError(command[0])

    monkeypatch.setattr(font_set.subprocess, "run", run)
    assert discover_fallback_fonts(("Present",)) == ()


def _pygame():
    pygame = pytest.importorskip("pygame")
    pygame.font.init()
    return pygame


def _default_font(pygame) -> Path:
    return Path(pygame.__file__).parent / pygame.font.get_default_font()


def test_the_primary_face_draws_what_it_has_unchanged():
    pygame = _pygame()
    fonts = FontSet(pygame, _default_font(pygame), 18)
    plain = pygame.font.Font(str(_default_font(pygame)), 18)
    white = (255, 255, 255)
    assert fonts.render("A", True, white).get_size() == plain.render("A", True, white).get_size()
    assert fonts.get_linesize() == plain.get_linesize()
    assert fonts.size("M") == (plain.size("M")[0], plain.get_linesize())


def test_a_character_no_face_has_shows_a_box_in_its_cells():
    pygame = _pygame()
    fonts = FontSet(pygame, _default_font(pygame), 18)
    # pygame's own face has no Han characters.
    box = fonts.render("\u4e2d", True, (255, 255, 255))
    assert fonts.face_for("\u4e2d") is None
    assert box.get_size() == (2 * fonts.cell_width, fonts.cell_height)
    assert box.get_bounding_rect().width > 0
    assert box.get_at((box.get_width() // 2, box.get_height() // 2)).a == 0


def test_labels_keep_each_face_s_advance():
    pygame = _pygame()
    fonts = FontSet(pygame, _default_font(pygame), 18, cells=False)
    plain = pygame.font.Font(str(_default_font(pygame)), 18)
    assert fonts.size("Wi")[0] == plain.size("W")[0] + plain.size("i")[0]


_NOTO = discover_fallback_fonts(("Noto Sans Mono CJK SC", "Noto Color Emoji"))


@pytest.mark.skipif(len(_NOTO) < 2, reason="Noto CJK and color emoji fonts are not installed")
def test_fallback_faces_draw_what_the_primary_lacks_fitted_to_their_cells():
    pygame = _pygame()
    fonts = FontSet(pygame, _default_font(pygame), 18, _NOTO)
    han = fonts.render("\u4e2d", True, (255, 255, 255))
    assert fonts.face_for("\u4e2d").path == _NOTO[0]
    assert han.get_size() == (2 * fonts.cell_width, fonts.cell_height)
    assert han.get_bounding_rect().width > 0

    # An emoji sequence is one glyph scaled into its two cells, and a color
    # glyph keeps its colors.
    family = "\U0001F468\u200d\U0001F469\u200d\U0001F467"
    sequence = fonts.render(family, True, (255, 255, 255))
    assert fonts.face_for(family).path == _NOTO[1]
    assert sequence.get_size() == (2 * fonts.cell_width, fonts.cell_height)
    assert sequence.get_bounding_rect().width > fonts.cell_width
    emoji = fonts.render("\U0001F600", True, (255, 255, 255))
    colors = {
        tuple(emoji.get_at((x, y)))[:3]
        for x in range(emoji.get_width())
        for y in range(emoji.get_height())
        if emoji.get_at((x, y)).a
    }
    assert any(len(set(color)) > 1 for color in colors)

    assert fonts.size("a\u4e2d" + family) == (5 * fonts.cell_width, fonts.cell_height)
    assert [path for path, _ in fonts.files()] == [
        str(_default_font(pygame)),
        *map(str, _NOTO),
    ]
