"""Viewer font sets: a fallback face per character, fitted to its cells."""

from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from rich_terminal import font_set
from rich_terminal.font_set import (
    FontSet,
    discover_fallback_fonts,
    discover_style_fonts,
)


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
    fonts = FontSet(pygame, _default_font(pygame), 18, _NOTO, styles={})
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


def test_style_discovery_keeps_only_real_faces_of_the_same_family(
    monkeypatch, tmp_path
):
    primary = tmp_path / "Mono.ttf"
    bold = tmp_path / "Mono-Bold.ttf"
    italic = tmp_path / "Mono-Italic.ttf"
    for path in (primary, bold, italic):
        path.write_bytes(b"font")

    def run(command, **_kwargs):
        if command[0] == "fc-query":
            return subprocess.CompletedProcess(command, 0, "Mono\n", "")
        pattern = command[-1]
        answers = {
            "Mono:weight=bold": f"{bold}\nMono\n200\n0",
            "Mono:slant=italic": f"{italic}\nMono\n80\n100",
            # Only another family answers: left out.
            "Mono:weight=bold:slant=italic": f"{bold}\nOther\n200\n100",
        }
        return subprocess.CompletedProcess(command, 0, answers[pattern], "")

    monkeypatch.setattr(font_set.subprocess, "run", run)
    assert discover_style_fonts(primary) == {"bold": bold, "italic": italic}


def test_style_discovery_without_fontconfig_finds_nothing(monkeypatch):
    def run(command, **_kwargs):
        raise FileNotFoundError(command[0])

    monkeypatch.setattr(font_set.subprocess, "run", run)
    assert discover_style_fonts("Mono.ttf") == {}


_DEJAVU_MONO = discover_fallback_fonts(("DejaVu Sans Mono",))


@pytest.mark.skipif(not _DEJAVU_MONO, reason="DejaVu Sans Mono is not installed")
def test_bold_and_italic_use_the_family_s_real_faces():
    pygame = _pygame()
    styles = discover_style_fonts(_DEJAVU_MONO[0])
    if "bold" not in styles:
        pytest.skip("DejaVu Sans Mono Bold is not installed")
    fonts = FontSet(pygame, _DEJAVU_MONO[0], 18, styles=styles)
    white = (255, 255, 255)
    real_bold = pygame.font.Font(str(styles["bold"]), 18).render("W", True, white)
    fonts.set_bold(True)
    drawn = fonts.render("W", True, white)
    fonts.set_bold(False)
    regular = fonts.render("W", True, white)
    # The real face draws bold glyphs, and the regular face is left plain.
    assert pygame.image.tobytes(drawn, "RGBA") == pygame.image.tobytes(real_bold, "RGBA")
    assert pygame.image.tobytes(regular, "RGBA") != pygame.image.tobytes(drawn, "RGBA")
    assert not fonts.faces[0].font.get_bold()
    assert [path for path, _ in fonts.files()][1:] == [
        str(styles[name]) for name in ("italic", "bold", "bold_italic") if name in styles
    ]
