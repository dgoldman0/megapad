"""Viewer fonts: a primary face and fallbacks, chosen per character.

APT-1-TEXT Section 6 draws a character's display scalars as one glyph
cluster fitted to its one or two cells.  One terminal font rarely has every
script, so a viewer keeps an ordered set of faces: its primary font, then
fallbacks such as Noto Sans CJK, Hebrew, Arabic, Symbols, and Color Emoji.
The first face with a glyph for every visible scalar of a character draws
it; an emoji character tries a color emoji face first.  A character no face
covers shows a box.

``FontSet`` offers the parts of the pygame font interface the painters use
(``render``, ``size``, ``get_linesize``, ``get_height``, and the italic and
bold switches), so it replaces one pygame font wherever that is passed.  In
cell mode each character is fitted to its cells, as the CELL grid, glyph
runs, and text areas need; otherwise each character keeps its face's
advance, as renderer-laid-out labels do.  Glyphs from the primary face are
returned exactly as that face renders them.
"""

from __future__ import annotations

import hashlib
import os
import subprocess
from dataclasses import dataclass
from pathlib import Path

from . import text_rules

# Font families a viewer falls back to, in order, when fontconfig has them.
DEFAULT_FALLBACK_FAMILIES = (
    "Noto Sans Mono CJK SC",
    "Noto Sans Hebrew",
    "Noto Sans Arabic",
    "Noto Sans Symbols",
    "Noto Sans Symbols2",
    "Noto Sans Math",
    "Noto Color Emoji",
)


def discover_fallback_fonts(families=DEFAULT_FALLBACK_FAMILIES) -> tuple[Path, ...]:
    """Each family's font file as fontconfig resolves it, in order.

    A family fontconfig can only substitute with another one is left out,
    as is every family when fontconfig is unavailable, so the set holds only
    the faces that were asked for.
    """

    found: list[Path] = []
    for family in families:
        try:
            result = subprocess.run(
                ["fc-match", "-f", "%{file}\n%{family[0]}", family],
                capture_output=True,
                text=True,
                timeout=5,
                check=False,
            )
        except (OSError, subprocess.SubprocessError):
            return tuple(found)
        path_text, _, resolved = result.stdout.partition("\n")
        if result.returncode != 0 or resolved.strip() != family or not path_text:
            continue
        path = Path(path_text)
        if path.is_file() and path not in found:
            found.append(path)
    return tuple(found)


def font_sha256(path: str | Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


@dataclass(slots=True)
class _Face:
    path: Path
    font: object              # pygame.font.Font
    coverage: object          # pygame.freetype.Font of the same file
    bitmap: bool              # fixed-size strikes only, as color emoji are
    primary: bool


class FontSet:
    """An ordered set of faces that draws each character with the first
    face that has it."""

    def __init__(
        self,
        pygame_module,
        primary: str | Path | None,
        size: int,
        fallbacks=(),
        *,
        cells: bool = True,
    ) -> None:
        import pygame.freetype as freetype

        if not freetype.get_init():
            freetype.init()
        self.pygame = pygame_module
        self.cells = cells
        self.size_points = size
        if primary is None:
            # The system's face for the kind of text, else pygame's own.
            primary = pygame_module.font.match_font("monospace" if cells else "sans")
            if primary is None:
                primary = os.path.join(
                    os.path.dirname(pygame_module.__file__),
                    pygame_module.font.get_default_font(),
                )
        primary_path = Path(primary)
        primary_font = pygame_module.font.Font(str(primary_path), size)
        primary_coverage = freetype.Font(str(primary_path), size)
        self.cell_width = max(1, primary_font.size("M")[0])
        self.cell_height = max(1, primary_font.get_linesize())
        self.faces = [_Face(primary_path, primary_font, primary_coverage, False, True)]
        for path in fallbacks:
            face = self._fallback_face(Path(path), size)
            if face is not None:
                self.faces.append(face)
        self._choice: dict[str, _Face | None] = {}
        self._italic = False
        self._bold = False

    def _fallback_face(self, path: Path, size: int) -> _Face | None:
        import pygame.freetype as freetype

        try:
            coverage = freetype.Font(str(path), size)
        except (OSError, self.pygame.error):
            return None
        if not coverage.scalable:
            # Color emoji come in fixed strikes; draw the first and scale
            # each glyph into its cells.
            sizes = coverage.get_sizes()
            if not sizes:
                return None
            strike = int(sizes[0][0])
            coverage = freetype.Font(str(path), strike)
            return _Face(path, self.pygame.font.Font(str(path), strike), coverage, True, False)
        # A scalable fallback is sized so its lines fit the primary's.
        probe = self.pygame.font.Font(str(path), size)
        fitted = max(1, size * self.cell_height // max(1, probe.get_linesize()))
        font = probe if fitted >= size else self.pygame.font.Font(str(path), fitted)
        return _Face(path, font, coverage, False, False)

    # -- the pygame font interface ----------------------------------------

    def get_linesize(self) -> int:
        return self.cell_height

    def get_height(self) -> int:
        return self.cell_height

    def get_italic(self) -> bool:
        return self._italic

    def set_italic(self, value: bool) -> None:
        self._italic = bool(value)
        for face in self.faces:
            if not face.bitmap:
                face.font.set_italic(self._italic)

    def get_bold(self) -> bool:
        return self._bold

    def set_bold(self, value: bool) -> None:
        self._bold = bool(value)
        for face in self.faces:
            if not face.bitmap:
                face.font.set_bold(self._bold)

    def size(self, text: str) -> tuple[int, int]:
        width = sum(self._advance(character) for character in self._characters(text))
        return width, self.cell_height

    def render(self, text: str, antialias: bool, color, background=None):
        characters = self._characters(text)
        if len(characters) == 1:
            return self._render_character(characters[0], antialias, color)
        surfaces = [
            self._render_character(character, antialias, color) for character in characters
        ]
        advances = [self._advance(character) for character in characters]
        line = self.pygame.Surface(
            (max(1, sum(advances)), self.cell_height), flags=self.pygame.SRCALPHA
        )
        x = 0
        for surface, advance in zip(surfaces, advances):
            line.blit(surface, (x, 0))
            x += advance
        return line

    def files(self) -> tuple[tuple[str, str], ...]:
        """Each face's file and its SHA-256, primary first, for evidence."""

        return tuple((str(face.path), font_sha256(face.path)) for face in self.faces)

    # -- faces and glyphs -------------------------------------------------

    @staticmethod
    def _characters(text: str) -> list[str]:
        if text.isascii():
            return list(text)
        return text_rules.characters(text, keep_tab=True)

    def face_for(self, character: str) -> _Face | None:
        """The face that draws CHARACTER, or None when no face has it."""

        if character in self._choice:
            return self._choice[character]
        chosen = None
        if character.isascii():
            chosen = self.faces[0]
        else:
            scalars = [ord(scalar) for scalar in character]
            visible = [
                cp for cp in scalars
                if not text_rules.default_ignorable(text_rules.props(cp))
            ] or scalars[:1]
            first = text_rules.props(scalars[0])
            emoji = text_rules.char_width(character) == 2 and (
                text_rules.extended_pictographic(first)
                or text_rules.gcb(first) == text_rules.GCB_RI
            )
            faces = sorted(self.faces, key=lambda face: not face.bitmap) if emoji else self.faces
            for face in faces:
                if self._covers(face, visible):
                    chosen = face
                    break
        self._choice[character] = chosen
        return chosen

    @staticmethod
    def _covers(face: _Face, scalars: list[int]) -> bool:
        metrics = face.coverage.get_metrics("".join(map(chr, scalars)))
        return bool(metrics) and all(metric is not None for metric in metrics)

    def _cells_of(self, character: str) -> int:
        return max(1, text_rules.char_width(character)) if not character.isascii() else 1

    def _advance(self, character: str) -> int:
        if self.cells:
            return self._cells_of(character) * self.cell_width
        face = self.face_for(character)
        if face is None:
            return max(1, self.cell_height * 3 // 5)
        width, height = face.font.size(character)
        if face.bitmap and height > self.cell_height:
            return max(1, width * self.cell_height // height)
        return max(1, width)

    def _render_character(self, character: str, antialias: bool, color):
        face = self.face_for(character)
        box_width = self._advance(character)
        if face is None:
            return self._missing_box(box_width, color)
        rgb = tuple(color)[:3]
        glyph = face.font.render(character, antialias, rgb)
        if face.primary and (not self.cells or glyph.get_width() <= box_width):
            return glyph
        return self._fit(glyph, box_width)

    def _fit(self, glyph, box_width: int):
        """GLYPH scaled down to fit a box BOX_WIDTH by one line, centred."""

        width, height = glyph.get_size()
        box_height = self.cell_height
        if width > box_width or height > box_height:
            scale_num, scale_den = min(
                (box_width, max(1, width)), (box_height, max(1, height)),
                key=lambda ratio: ratio[0] / ratio[1],
            )
            width = max(1, width * scale_num // scale_den)
            height = max(1, height * scale_num // scale_den)
            scale = (
                self.pygame.transform.smoothscale
                if glyph.get_bitsize() in (24, 32)
                else self.pygame.transform.scale
            )
            glyph = scale(glyph, (width, height))
        box = self.pygame.Surface((box_width, box_height), flags=self.pygame.SRCALPHA)
        box.blit(glyph, ((box_width - width) // 2, (box_height - height) // 2))
        return box

    def _missing_box(self, box_width: int, color):
        """A character no face has shows an outlined box in its cells."""

        box = self.pygame.Surface((box_width, self.cell_height), flags=self.pygame.SRCALPHA)
        inset = self.pygame.Rect(0, 0, box_width, self.cell_height).inflate(-2, -4)
        if inset.width > 0 and inset.height > 0:
            self.pygame.draw.rect(box, tuple(color)[:3], inset, 1)
        return box
