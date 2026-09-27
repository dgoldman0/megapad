#!/usr/bin/env python3
"""Generate MegaPad's Unicode text data from the Unicode 15.1.0 UCD.

The shared text contract (docs/rich-terminal/APT-1-TEXT.md) pins Unicode
15.1.0 and the exact data files below.  This script is the only way
``rich_terminal/text_data.py`` is made; it is never edited by hand.

    python3 generate_text_data.py [--ucd DIR] [--check]

``--check`` exits nonzero when the checked-in module differs from what the
data produces, so a test can prove the data is current.
"""

from __future__ import annotations

import argparse
import hashlib
import re
import sys
from pathlib import Path


UNICODE_VERSION = "15.1.0"
DEFAULT_UCD = Path("/usr/share/unicode")
MEGAPAD_ROOT = Path(__file__).resolve().parent
OUTPUT = MEGAPAD_ROOT / "rich_terminal" / "text_data.py"
MAX_SCALAR = 0x10FFFF

# Field layout of one packed property value.  rich_terminal/text_rules.py
# decodes these exact shifts and widths; keep the two in step.
GCB_NAMES = (
    "Other", "CR", "LF", "Control", "Extend", "ZWJ", "Regional_Indicator",
    "Prepend", "SpacingMark", "L", "V", "T", "LV", "LVT",
)
INCB_NAMES = (None, "Consonant", "Extend", "Linker")
BIDI_NAMES = (
    "L", "R", "AL", "EN", "ES", "ET", "AN", "CS", "NSM", "BN", "B", "S",
    "WS", "ON", "LRE", "LRO", "RLE", "RLO", "PDF", "LRI", "RLI", "FSI", "PDI",
)
JOINING_NAMES = ("U", "D", "R", "L", "C", "T")

SHIFT_GCB = 0          # 4 bits
BIT_EXTPICT = 4
SHIFT_INCB = 5         # 2 bits
SHIFT_WIDTH = 7        # 2 bits: scalar width 0, 1, or 2
BIT_EMOJI = 9
BIT_EMOJI_MODIFIER = 10
BIT_DEFAULT_IGNORABLE = 11
BIT_INVALID = 12       # Cc, Zl, Zp: replaced by U+FFFD before display
SHIFT_BIDI = 13        # 5 bits
SHIFT_JOINING = 18     # 3 bits
BIT_MIRROR = 21        # has a Bidi_Mirroring_Glyph
BIT_BRACKET = 22       # has a Bidi_Paired_Bracket_Type other than None
BIT_ARABIC_FORMS = 23  # has at least one Arabic presentation form

BIDI_LONG = {
    "Left_To_Right": "L", "Right_To_Left": "R", "Arabic_Letter": "AL",
    "European_Number": "EN", "European_Separator": "ES",
    "European_Terminator": "ET", "Arabic_Number": "AN",
    "Common_Separator": "CS", "Nonspacing_Mark": "NSM",
    "Boundary_Neutral": "BN", "Paragraph_Separator": "B",
    "Segment_Separator": "S", "White_Space": "WS", "Other_Neutral": "ON",
    "Left_To_Right_Embedding": "LRE", "Left_To_Right_Override": "LRO",
    "Right_To_Left_Embedding": "RLE", "Right_To_Left_Override": "RLO",
    "Pop_Directional_Format": "PDF", "Left_To_Right_Isolate": "LRI",
    "Right_To_Left_Isolate": "RLI", "First_Strong_Isolate": "FSI",
    "Pop_Directional_Isolate": "PDI",
}
JOINING_LONG = {
    "Non_Joining": "U", "Dual_Joining": "D", "Right_Joining": "R",
    "Left_Joining": "L", "Join_Causing": "C", "Transparent": "T",
}
EAW_LONG = {
    "Neutral": "N", "Ambiguous": "A", "Halfwidth": "H", "Fullwidth": "F",
    "Narrow": "Na", "Wide": "W",
}
FORM_TAGS = ("<isolated>", "<final>", "<initial>", "<medial>")


class UcdError(RuntimeError):
    pass


# SHA-256 of each pinned Unicode 15.1.0 file, as published at
# https://www.unicode.org/Public/15.1.0/ucd/.  A header line is not proof of
# version (UnicodeData.txt has none), so the exact bytes are pinned instead.
UCD_SHA256 = {
    "UnicodeData.txt":
        "2fc713e6a31a87c4850a37fe2caffa4218180fadb5de86b43a143ddb4581fb86",
    "EastAsianWidth.txt":
        "b08191401dc125f4e84ef262a95754faae6b737c79538e17ea9664a63434e94e",
    "HangulSyllableType.txt":
        "8539c7fdb3e8cd9efcc04a420061fa56a74762a3cb60fc9962c26093c8944427",
    "DerivedCoreProperties.txt":
        "f55d0db69123431a7317868725b1fcbf1eab6b265d756d1bd7f0f6d9f9ee108b",
    "auxiliary/GraphemeBreakProperty.txt":
        "a7e52eee647e52dc210b8719b4d7037276f4b353810293d69377fc46374cec3f",
    "emoji/emoji-data.txt":
        "d7aef489c8fe4c14f09ea5695200277c6b93ac82ac60845cdd2161b0d6835cc1",
    "extracted/DerivedBidiClass.txt":
        "b57884c59a3a5348d86faed39965bbddb4e4493d3c16c1ec378ec26bfb5821be",
    "BidiBrackets.txt":
        "2f0531dc6a2aafa7462c7f89008c2845ade52e51267ac5ba39339adeefb8fe53",
    "BidiMirroring.txt":
        "8116a6eea6a7ff8c15b2cfda2d50cfe92fe90d05f95145daf09a510595a65aa5",
    "extracted/DerivedJoiningType.txt":
        "2e0ed3733272299007cf0b76e84a8a653192d99a4429d2232fffcabccfd2d462",
    "auxiliary/GraphemeBreakTest.txt":
        "ed9c5e92fd0911ccbeeb63c97cb19c519ea272ff1112ce843abd991582dd848f",
    "BidiTest.txt":
        "23413900dddcd2a246440f1f0cf27a3e1e62b3d0aebfa8f44ce241f5950fe956",
    "BidiCharacterTest.txt":
        "18507f7ec57ec8155b7771dbc88f3e3201ad08eba4dba70e4e095ef628020fa8",
}


def ucd_file(root: Path, name: str) -> Path:
    """Return a pinned UCD file after proving its exact Unicode 15.1.0 bytes."""

    path = root / name
    try:
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
    except FileNotFoundError as exc:
        raise UcdError(
            f"{path} is missing; install the Unicode {UNICODE_VERSION} UCD "
            "(Debian/Ubuntu package unicode-data) or pass --ucd"
        ) from exc
    if digest != UCD_SHA256[name]:
        raise UcdError(f"{path} is not the pinned Unicode {UNICODE_VERSION} file")
    return path


def _range(text: str) -> range:
    if ".." in text:
        first, last = text.split("..")
        return range(int(first, 16), int(last, 16) + 1)
    value = int(text, 16)
    return range(value, value + 1)


def _data_lines(path: Path):
    for line in path.read_text(encoding="utf-8").splitlines():
        body = line.split("#", 1)[0].strip()
        if body:
            yield [field.strip() for field in body.split(";")]


def _missing(path: Path) -> list[tuple[range, str]]:
    """Return the file's ``@missing`` defaults in order; later lines win."""

    result = []
    pattern = re.compile(r"#\s*@missing:\s*([0-9A-F.]+);\s*([^;#]+)")
    for line in path.read_text(encoding="utf-8").splitlines():
        match = pattern.match(line)
        if match:
            result.append((_range(match[1]), match[2].strip()))
    return result


def _with_defaults(path: Path, field: int = 1, names=None) -> list[str]:
    values: list[str | None] = [None] * (MAX_SCALAR + 1)
    for span, value in _missing(path):
        value = names.get(value, value) if names else value
        for cp in span:
            values[cp] = value
    for fields in _data_lines(path):
        value = fields[field]
        value = names.get(value, value) if names else value
        for cp in _range(fields[0]):
            values[cp] = value
    return values  # type: ignore[return-value]


class Ucd:
    """The properties APT-1-TEXT Section 2 names, one value per scalar."""

    def __init__(self, root: Path) -> None:
        self.root = root
        self.general_category = self._general_category()
        self.east_asian_width = _with_defaults(
            ucd_file(root, "EastAsianWidth.txt"), names=EAW_LONG
        )
        self.hangul = [None] * (MAX_SCALAR + 1)
        for fields in _data_lines(ucd_file(root, "HangulSyllableType.txt")):
            for cp in _range(fields[0]):
                self.hangul[cp] = fields[1]
        self.grapheme = ["Other"] * (MAX_SCALAR + 1)
        for fields in _data_lines(ucd_file(root, "auxiliary/GraphemeBreakProperty.txt")):
            for cp in _range(fields[0]):
                self.grapheme[cp] = fields[1]
        self.incb = [None] * (MAX_SCALAR + 1)
        self.default_ignorable = [False] * (MAX_SCALAR + 1)
        for fields in _data_lines(ucd_file(root, "DerivedCoreProperties.txt")):
            if fields[1] == "InCB":
                for cp in _range(fields[0]):
                    self.incb[cp] = fields[2]
            elif fields[1] == "Default_Ignorable_Code_Point":
                for cp in _range(fields[0]):
                    self.default_ignorable[cp] = True
        self.emoji: dict[str, set[int]] = {}
        for fields in _data_lines(ucd_file(root, "emoji/emoji-data.txt")):
            bucket = self.emoji.setdefault(fields[1], set())
            bucket.update(_range(fields[0]))
        self.bidi = _with_defaults(
            ucd_file(root, "extracted/DerivedBidiClass.txt"), names=BIDI_LONG
        )
        self.joining = _with_defaults(
            ucd_file(root, "extracted/DerivedJoiningType.txt"), names=JOINING_LONG
        )
        self.mirror: dict[int, int] = {}
        for fields in _data_lines(ucd_file(root, "BidiMirroring.txt")):
            self.mirror[int(fields[0], 16)] = int(fields[1], 16)
        self.brackets: dict[int, tuple[int, str]] = {}
        for fields in _data_lines(ucd_file(root, "BidiBrackets.txt")):
            self.brackets[int(fields[0], 16)] = (int(fields[1], 16), fields[2])
        self.arabic_forms = self._arabic_forms()

    def _general_category(self) -> list[str]:
        values = ["Cn"] * (MAX_SCALAR + 1)
        first = None
        for fields in _data_lines(ucd_file(self.root, "UnicodeData.txt")):
            cp = int(fields[0], 16)
            name = fields[1]
            if name.endswith(", First>"):
                first = cp
                continue
            if name.endswith(", Last>"):
                assert first is not None
                for scalar in range(first, cp + 1):
                    values[scalar] = fields[2]
                first = None
                continue
            values[cp] = fields[2]
        return values

    def _arabic_forms(self) -> dict[int, list[int]]:
        """Map each nominal letter to [isolated, final, initial, medial]."""

        forms: dict[int, list[int]] = {}
        for fields in _data_lines(ucd_file(self.root, "UnicodeData.txt")):
            cp = int(fields[0], 16)
            if not (0xFB50 <= cp <= 0xFDFF or 0xFE70 <= cp <= 0xFEFF):
                continue
            decomposition = fields[5].split()
            if len(decomposition) != 2 or decomposition[0] not in FORM_TAGS:
                continue
            base = int(decomposition[1], 16)
            slot = FORM_TAGS.index(decomposition[0])
            entry = forms.setdefault(base, [0, 0, 0, 0])
            # Several presentation forms can decompose to one letter and form
            # (for example compatibility duplicates); the first one wins.
            if entry[slot] == 0:
                entry[slot] = cp
        return forms

    def scalar_width(self, cp: int) -> int:
        category = self.general_category[cp]
        if category in ("Mn", "Me", "Cf") or self.hangul[cp] in ("V", "T"):
            return 0
        if self.east_asian_width[cp] in ("W", "F"):
            return 2
        return 1

    def packed(self, cp: int) -> int:
        category = self.general_category[cp]
        value = GCB_NAMES.index(self.grapheme[cp]) << SHIFT_GCB
        if cp in self.emoji["Extended_Pictographic"]:
            value |= 1 << BIT_EXTPICT
        value |= INCB_NAMES.index(self.incb[cp]) << SHIFT_INCB
        value |= self.scalar_width(cp) << SHIFT_WIDTH
        if cp in self.emoji["Emoji"]:
            value |= 1 << BIT_EMOJI
        if cp in self.emoji["Emoji_Modifier"]:
            value |= 1 << BIT_EMOJI_MODIFIER
        if self.default_ignorable[cp]:
            value |= 1 << BIT_DEFAULT_IGNORABLE
        if category in ("Cc", "Zl", "Zp"):
            value |= 1 << BIT_INVALID
        value |= BIDI_NAMES.index(self.bidi[cp]) << SHIFT_BIDI
        value |= JOINING_NAMES.index(self.joining[cp]) << SHIFT_JOINING
        if cp in self.mirror:
            value |= 1 << BIT_MIRROR
        if cp in self.brackets:
            value |= 1 << BIT_BRACKET
        if cp in self.arabic_forms:
            value |= 1 << BIT_ARABIC_FORMS
        return value


def property_ranges(ucd: Ucd) -> tuple[list[int], list[tuple[int, int]]]:
    """Return distinct packed values and (first scalar, value index) runs."""

    values: list[int] = []
    index_of: dict[int, int] = {}
    runs: list[tuple[int, int]] = []
    previous = None
    for cp in range(MAX_SCALAR + 1):
        packed = ucd.packed(cp)
        if packed == previous:
            continue
        previous = packed
        if packed not in index_of:
            index_of[packed] = len(values)
            values.append(packed)
        runs.append((cp, index_of[packed]))
    return values, runs


def canonical_bracket(cp: int) -> int:
    """BD16 compares brackets through their canonical equivalents."""

    return {0x2329: 0x3008, 0x232A: 0x3009}.get(cp, cp)


def _wrapped(items: list[str], indent: str = "    ", width: int = 79) -> list[str]:
    lines, line = [], indent
    for item in items:
        piece = item + ", "
        if len(line) + len(piece.rstrip()) > width and line.strip():
            lines.append(line.rstrip())
            line = indent
        line += piece
    if line.strip():
        lines.append(line.rstrip())
    return lines


def render(ucd: Ucd) -> str:
    for cp in range(MAX_SCALAR + 1):
        if (ucd.incb[cp] == "Consonant" or cp in ucd.emoji["Extended_Pictographic"]) and (
            ucd.grapheme[cp] != "Other"
        ):
            raise UcdError(f"U+{cp:04X} breaks text_rules.py's pair table assumption")
    values, runs = property_ranges(ucd)
    out = [
        '"""Unicode 15.1.0 text data for the shared text contract.',
        "",
        "GENERATED by generate_text_data.py from the Unicode 15.1.0 Character",
        "Database named in docs/rich-terminal/APT-1-TEXT.md Section 2.  Do not edit.",
        "",
        "VALUES holds each distinct packed property value; RUN_FIRSTS and",
        "RUN_VALUES give, for each run of equal values in scalar order, its first",
        "scalar and its index into VALUES.  text_rules.py decodes the fields.",
        '"""',
        "",
        f'UNICODE_VERSION = "{UNICODE_VERSION}"',
        "",
        "VALUES = (",
        *_wrapped([f"0x{value:06X}" for value in values]),
        ")",
        "",
        "RUN_FIRSTS = (",
        *_wrapped([f"0x{first:X}" for first, _ in runs]),
        ")",
        "",
        "RUN_VALUES = (",
        *_wrapped([str(index) for _, index in runs]),
        ")",
        "",
        "# Bidi_Mirroring_Glyph.",
        "MIRRORS = {",
        *_wrapped([f"0x{cp:X}: 0x{glyph:X}" for cp, glyph in sorted(ucd.mirror.items())]),
        "}",
        "",
        "# Bidi_Paired_Bracket (through its canonical equivalent, as BD16",
        "# compares them) and Bidi_Paired_Bracket_Type: 1 open, 2 close.",
        "BRACKETS = {",
        *_wrapped([
            f"0x{cp:X}: (0x{canonical_bracket(pair):X}, {1 if kind == 'o' else 2})"
            for cp, (pair, kind) in sorted(ucd.brackets.items())
        ]),
        "}",
        "",
        "# Arabic presentation forms: isolated, final, initial, medial (0 = none).",
        "ARABIC_FORMS = {",
        *_wrapped([
            f"0x{base:X}: (0x{isolated:X}, 0x{final:X}, 0x{initial:X}, 0x{medial:X})"
            for base, (isolated, final, initial, medial) in sorted(ucd.arabic_forms.items())
        ]),
        "}",
        "",
    ]
    return "\n".join(out)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ucd", type=Path, default=DEFAULT_UCD)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args(argv)
    text = render(Ucd(args.ucd))
    if args.check:
        current = args.output.read_text(encoding="utf-8") if args.output.exists() else ""
        if current != text:
            print(f"{args.output} is stale; rerun the generator", file=sys.stderr)
            return 1
        print(f"{args.output} matches Unicode {UNICODE_VERSION}")
        return 0
    args.output.write_text(text, encoding="utf-8")
    print(f"wrote {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
