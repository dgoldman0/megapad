"""The shared text rules of ``docs/rich-terminal/APT-1-TEXT.md``.

This module is the terminal's implementation of the contract: characters
(extended grapheme clusters, UAX #29), widths, invalid-input replacement,
the Unicode Bidirectional Algorithm (UAX #9) at the character level, Arabic
joining on the cell grid, and the layout of one row of logical text into
visual cells.  All data comes from :mod:`rich_terminal.text_data`, which is
generated from the pinned Unicode 15.1.0 files.

The client (Akashic) implements the same rules independently.  Both are
checked against Unicode's own conformance files.
"""

from __future__ import annotations

from bisect import bisect_right
from dataclasses import dataclass
from functools import lru_cache

from . import text_data


# ---------------------------------------------------------------------------
# Properties
# ---------------------------------------------------------------------------

GCB_OTHER, GCB_CR, GCB_LF, GCB_CONTROL, GCB_EXTEND, GCB_ZWJ, GCB_RI = range(7)
GCB_PREPEND, GCB_SPACINGMARK, GCB_L, GCB_V, GCB_T, GCB_LV, GCB_LVT = range(7, 14)
INCB_NONE, INCB_CONSONANT, INCB_EXTEND, INCB_LINKER = range(4)

(
    BC_L, BC_R, BC_AL, BC_EN, BC_ES, BC_ET, BC_AN, BC_CS, BC_NSM, BC_BN, BC_B,
    BC_S, BC_WS, BC_ON, BC_LRE, BC_LRO, BC_RLE, BC_RLO, BC_PDF, BC_LRI, BC_RLI,
    BC_FSI, BC_PDI,
) = range(23)
BIDI_NAMES = (
    "L", "R", "AL", "EN", "ES", "ET", "AN", "CS", "NSM", "BN", "B", "S", "WS",
    "ON", "LRE", "LRO", "RLE", "RLO", "PDF", "LRI", "RLI", "FSI", "PDI",
)

JT_U, JT_D, JT_R, JT_L, JT_C, JT_T = range(6)
FORM_ISOLATED, FORM_FINAL, FORM_INITIAL, FORM_MEDIAL = range(4)

REPLACEMENT = 0xFFFD
VARIATION_SELECTOR_16 = 0xFE0F


def props(cp: int) -> int:
    """Packed properties of one scalar; out-of-range values read as U+FFFD."""

    if not 0 <= cp <= 0x10FFFF:
        cp = REPLACEMENT
    return text_data.VALUES[text_data.RUN_VALUES[bisect_right(text_data.RUN_FIRSTS, cp) - 1]]


def gcb(p: int) -> int:
    return p & 15


def extended_pictographic(p: int) -> bool:
    return bool(p & 16)


def incb(p: int) -> int:
    return (p >> 5) & 3


def scalar_width(p: int) -> int:
    """``w(s)``: 0, 1, or 2."""

    return (p >> 7) & 3


def emoji(p: int) -> bool:
    return bool(p & (1 << 9))


def emoji_modifier(p: int) -> bool:
    return bool(p & (1 << 10))


def default_ignorable(p: int) -> bool:
    return bool(p & (1 << 11))


def invalid(p: int) -> bool:
    """General_Category Cc, Zl, or Zp: replaced before display."""

    return bool(p & (1 << 12))


def bidi_class(p: int) -> int:
    return (p >> 13) & 31


def joining_type(p: int) -> int:
    return (p >> 18) & 7


def mirror(cp: int) -> int:
    return text_data.MIRRORS.get(cp, cp)


def bracket(cp: int) -> tuple[int, int]:
    """(paired bracket, type) with type 1 open, 2 close; (0, 0) if none."""

    return text_data.BRACKETS.get(cp, (0, 0))


def arabic_form(cp: int, form: int) -> int:
    """The presentation form scalar for ``form``, or 0 when there is none."""

    forms = text_data.ARABIC_FORMS.get(cp)
    return forms[form] if forms else 0


# ---------------------------------------------------------------------------
# Section 5: invalid input
# ---------------------------------------------------------------------------

def display_scalars(text: str | bytes, *, keep_tab: bool = False) -> list[int]:
    """Decode with one U+FFFD per maximal ill-formed subpart, then replace
    Cc, Zl, and Zp scalars (except a tab when ``keep_tab``)."""

    if isinstance(text, (bytes, bytearray, memoryview)):
        text = bytes(text).decode("utf-8", errors="replace")
    result = []
    for character in text:
        cp = ord(character)
        if 0xD800 <= cp <= 0xDFFF or (invalid(props(cp)) and not (keep_tab and cp == 9)):
            cp = REPLACEMENT
        result.append(cp)
    return result


# ---------------------------------------------------------------------------
# Section 3: characters (UAX #29 extended grapheme clusters)
# ---------------------------------------------------------------------------

def _pair_action(prev: int, cur: int) -> int:
    """0 break, 1 no break, 2 unless GB9c/GB11, 3 unless GB12/GB13."""

    if prev == GCB_CR and cur == GCB_LF:
        return 1
    if prev in (GCB_CR, GCB_LF, GCB_CONTROL) or cur in (GCB_CR, GCB_LF, GCB_CONTROL):
        return 0
    if prev == GCB_L and cur in (GCB_L, GCB_V, GCB_LV, GCB_LVT):
        return 1
    if prev in (GCB_LV, GCB_V) and cur in (GCB_V, GCB_T):
        return 1
    if prev in (GCB_LVT, GCB_T) and cur == GCB_T:
        return 1
    if cur in (GCB_EXTEND, GCB_ZWJ, GCB_SPACINGMARK) or prev == GCB_PREPEND:
        return 1
    if prev in (GCB_EXTEND, GCB_ZWJ) and cur == GCB_OTHER:
        return 2
    if prev == cur == GCB_RI:
        return 3
    return 0


_PAIR_ACTIONS = tuple(tuple(_pair_action(p, c) for c in range(14)) for p in range(14))


class GraphemeSegmenter:
    """Judge one scalar at a time whether a character boundary precedes it."""

    __slots__ = ("prev", "ri", "emoji", "incb")

    def __init__(self) -> None:
        self.prev = -1
        self.ri = 0
        self.emoji = 0
        self.incb = 0

    def breaks_before(self, p: int) -> bool:
        cur = gcb(p)
        prev = self.prev
        if prev < 0:
            result = True
        else:
            action = _PAIR_ACTIONS[prev][cur]
            if action == 0:
                result = True
            elif action == 1:
                result = False
            elif action == 2:
                result = not (
                    (incb(p) == INCB_CONSONANT and self.incb == 2)
                    or (extended_pictographic(p) and self.emoji == 2)
                )
            else:
                result = self.ri % 2 == 0
        self.ri = self.ri + 1 if cur == GCB_RI else 0
        if extended_pictographic(p):
            self.emoji = 1
        elif self.emoji == 1 and cur == GCB_EXTEND:
            self.emoji = 1
        elif self.emoji == 1 and cur == GCB_ZWJ:
            self.emoji = 2
        else:
            self.emoji = 0
        category = incb(p)
        if category == INCB_CONSONANT:
            self.incb = 1
        elif self.incb and category == INCB_LINKER:
            self.incb = 2
        elif not (self.incb and category == INCB_EXTEND):
            self.incb = 0
        self.prev = cur
        return result


def segment(scalars: list[int]) -> list[tuple[int, int]]:
    """Characters as (start, length) scalar spans, in logical order."""

    segmenter = GraphemeSegmenter()
    spans: list[tuple[int, int]] = []
    for index, cp in enumerate(scalars):
        if segmenter.breaks_before(props(cp)) or not spans:
            spans.append((index, 1))
        else:
            start, length = spans[-1]
            spans[-1] = (start, length + 1)
    return spans


def char_width(cluster: list[int] | tuple[int, ...] | str) -> int:
    """Section 4, ``W(c)``, for one character's scalars."""

    if isinstance(cluster, str):
        cluster = [ord(ch) for ch in cluster]
    first = props(cluster[0])
    if all(default_ignorable(props(cp)) for cp in cluster):
        return 0
    if len(cluster) > 1:
        second = props(cluster[1])
        if gcb(first) == GCB_RI and gcb(second) == GCB_RI:
            return 2
        if emoji(first) and (cluster[1] == VARIATION_SELECTOR_16 or emoji_modifier(second)):
            return 2
    width = scalar_width(first)
    return width if width else 1


def characters(text: str | bytes, *, keep_tab: bool = False) -> list[str]:
    scalars = display_scalars(text, keep_tab=keep_tab)
    return ["".join(map(chr, scalars[s:s + n])) for s, n in segment(scalars)]


def string_width(text: str | bytes, *, keep_tab: bool = False) -> int:
    return sum(char_width(ch) for ch in characters(text, keep_tab=keep_tab))


# ---------------------------------------------------------------------------
# Section 7: the Unicode Bidirectional Algorithm (UAX #9, Unicode 15.1.0)
# ---------------------------------------------------------------------------

MAX_DEPTH = 125
DIRECTION_AUTO, DIRECTION_LTR, DIRECTION_RTL = 0, 1, 2

_ISOLATE_INITIATORS = frozenset((BC_LRI, BC_RLI, BC_FSI))
_REMOVED = frozenset((BC_RLE, BC_LRE, BC_RLO, BC_LRO, BC_PDF, BC_BN))
_NEUTRALS = frozenset((BC_B, BC_S, BC_WS, BC_ON, BC_LRI, BC_RLI, BC_FSI, BC_PDI))
_STRONG_R = frozenset((BC_R, BC_AL))


def _first_strong(classes: list[int], start: int, end: int) -> int | None:
    """P2/P3: 0 for L, 1 for R or AL, None when no strong character."""

    depth = 0
    for index in range(start, end):
        kind = classes[index]
        if kind in _ISOLATE_INITIATORS:
            depth += 1
        elif kind == BC_PDI:
            if depth:
                depth -= 1
        elif kind == BC_B:
            break
        elif depth == 0:
            if kind == BC_L:
                return 0
            if kind in _STRONG_R:
                return 1
    return None


def _matching_pdis(classes: list[int]) -> dict[int, int]:
    """BD9: isolate initiator index -> index of its matching PDI."""

    matches: dict[int, int] = {}
    stack: list[int] = []
    for index, kind in enumerate(classes):
        if kind in _ISOLATE_INITIATORS:
            stack.append(index)
        elif kind == BC_PDI and stack:
            matches[stack.pop()] = index
        elif kind == BC_B:
            stack.clear()
    return matches


def _direction(level: int) -> int:
    return BC_R if level & 1 else BC_L


def bidi_levels(
    classes: list[int],
    direction: int = DIRECTION_AUTO,
    scalars: list[int] | None = None,
) -> tuple[list[int | None], int]:
    """Resolve embedding levels for one paragraph.

    ``classes`` are Bidi_Class values; ``scalars`` supply the paired brackets
    for rule N0 and may be omitted when no character is a bracket.  Returns
    the per-character levels, with ``None`` for characters removed by X9, and
    the paragraph embedding level.
    """

    n = len(classes)
    if direction == DIRECTION_AUTO:
        paragraph = _first_strong(classes, 0, n) or 0
    else:
        paragraph = 1 if direction == DIRECTION_RTL else 0
    matches = _matching_pdis(classes)
    matched_pdis = set(matches.values())

    # X1 to X8: explicit levels and overrides.
    levels = [paragraph] * n
    types = list(classes)
    stack = [(paragraph, None, False)]  # (level, override, isolate)
    overflow_isolates = 0
    overflow_embeddings = 0
    valid_isolates = 0
    for index, kind in enumerate(classes):
        level, override, _ = stack[-1]
        if kind in (BC_RLE, BC_LRE, BC_RLO, BC_LRO):
            rtl = kind in (BC_RLE, BC_RLO)
            new_level = (level + 1) | 1 if rtl else (level + 2) & ~1
            if new_level <= MAX_DEPTH and not overflow_isolates and not overflow_embeddings:
                new_override = {BC_RLO: BC_R, BC_LRO: BC_L}.get(kind)
                stack.append((new_level, new_override, False))
            elif not overflow_isolates:
                overflow_embeddings += 1
            levels[index] = level
        elif kind in _ISOLATE_INITIATORS:
            levels[index] = level
            if override is not None:
                types[index] = override
            if kind == BC_FSI:
                end = matches.get(index, n)
                rtl = _first_strong(classes, index + 1, end) == 1
            else:
                rtl = kind == BC_RLI
            new_level = (level + 1) | 1 if rtl else (level + 2) & ~1
            if new_level <= MAX_DEPTH and not overflow_isolates and not overflow_embeddings:
                valid_isolates += 1
                stack.append((new_level, None, True))
            else:
                overflow_isolates += 1
        elif kind == BC_PDI:
            if overflow_isolates:
                overflow_isolates -= 1
            elif valid_isolates:
                overflow_embeddings = 0
                while not stack[-1][2]:
                    stack.pop()
                stack.pop()
                valid_isolates -= 1
            level, override, _ = stack[-1]
            levels[index] = level
            if override is not None:
                types[index] = override
        elif kind == BC_PDF:
            if overflow_isolates:
                pass
            elif overflow_embeddings:
                overflow_embeddings -= 1
            elif not stack[-1][2] and len(stack) >= 2:
                stack.pop()
            levels[index] = level
        elif kind == BC_B:
            levels[index] = paragraph
        elif kind == BC_BN:
            levels[index] = level
        else:
            levels[index] = level
            if override is not None:
                types[index] = override

    # X9: removed characters take no part in what follows.
    removed = [kind in _REMOVED for kind in classes]

    # X10: level runs chained into isolating run sequences.
    runs: list[list[int]] = []
    for index in range(n):
        if removed[index]:
            continue
        if runs and levels[runs[-1][-1]] == levels[index]:
            runs[-1].append(index)
        else:
            runs.append([index])
    run_starting = {run[0]: run for run in runs}
    sequences: list[list[int]] = []
    for run in runs:
        if classes[run[0]] == BC_PDI and run[0] in matched_pdis:
            continue
        sequence = list(run)
        while classes[sequence[-1]] in _ISOLATE_INITIATORS and sequence[-1] in matches:
            continuation = run_starting.get(matches[sequence[-1]])
            if continuation is None:
                break
            sequence.extend(continuation)
        sequences.append(sequence)

    # sos and eos compare explicit embedding levels (X10), so later
    # sequences must not see levels that earlier ones already resolved.
    explicit = list(levels)
    for sequence in sequences:
        _resolve_sequence(
            sequence, classes, types, levels, explicit, removed, matches, paragraph, scalars
        )

    # L1: separators, and whitespace before them or at the end, reset.
    trailing = True
    for index in range(n - 1, -1, -1):
        kind = classes[index]
        if kind in (BC_S, BC_B):
            levels[index] = paragraph
            trailing = True
        elif kind in (BC_WS, BC_FSI, BC_LRI, BC_RLI, BC_PDI) or removed[index]:
            if trailing:
                levels[index] = paragraph
        else:
            trailing = False

    return [None if removed[i] else levels[i] for i in range(n)], paragraph


def _neighbour_level(levels, removed, index, step, n, paragraph) -> int:
    index += step
    while 0 <= index < n:
        if not removed[index]:
            return levels[index]
        index += step
    return paragraph


def _resolve_sequence(
    sequence, classes, types, levels, explicit, removed, matches, paragraph, scalars
):
    n = len(classes)
    level = explicit[sequence[0]]
    first, last = sequence[0], sequence[-1]
    sos = _direction(max(level, _neighbour_level(explicit, removed, first, -1, n, paragraph)))
    if classes[last] in _ISOLATE_INITIATORS and last not in matches:
        after = paragraph
    else:
        after = _neighbour_level(explicit, removed, last, 1, n, paragraph)
    eos = _direction(max(explicit[last], after))
    t = [types[i] for i in sequence]
    count = len(t)

    # W1: NSM takes the type before it, or ON after an isolate boundary.
    for k in range(count):
        if t[k] == BC_NSM:
            if k == 0:
                t[k] = sos
            elif t[k - 1] in (BC_LRI, BC_RLI, BC_FSI, BC_PDI):
                t[k] = BC_ON
            else:
                t[k] = t[k - 1]
    # W2: EN after AL becomes AN.
    strong = sos
    for k in range(count):
        if t[k] in (BC_L, BC_R, BC_AL):
            strong = t[k]
        elif t[k] == BC_EN and strong == BC_AL:
            t[k] = BC_AN
    # W3: AL becomes R.
    for k in range(count):
        if t[k] == BC_AL:
            t[k] = BC_R
    # W4: one separator between two numbers of the same kind.
    for k in range(1, count - 1):
        if t[k] == BC_ES and t[k - 1] == BC_EN and t[k + 1] == BC_EN:
            t[k] = BC_EN
        elif t[k] == BC_CS and t[k - 1] == t[k + 1] and t[k - 1] in (BC_EN, BC_AN):
            t[k] = t[k - 1]
    # W5: terminators next to EN become EN.
    k = 0
    while k < count:
        if t[k] == BC_ET:
            end = k
            while end < count and t[end] == BC_ET:
                end += 1
            if (k > 0 and t[k - 1] == BC_EN) or (end < count and t[end] == BC_EN):
                for j in range(k, end):
                    t[j] = BC_EN
            k = end
        else:
            k += 1
    # W6: remaining separators and terminators become ON.
    for k in range(count):
        if t[k] in (BC_ES, BC_ET, BC_CS):
            t[k] = BC_ON
    # W7: EN after L (or an L sos) becomes L.
    strong = sos
    for k in range(count):
        if t[k] in (BC_L, BC_R):
            strong = t[k]
        elif t[k] == BC_EN and strong == BC_L:
            t[k] = BC_L

    embedding = _direction(level)
    if scalars is not None:
        _resolve_brackets(sequence, t, classes, scalars, sos, embedding)

    # N1 and N2: neutrals take matching surrounding strong text, else e.
    def strength(kind):
        if kind == BC_L:
            return BC_L
        if kind in (BC_R, BC_EN, BC_AN):
            return BC_R
        return None

    k = 0
    while k < count:
        if t[k] in _NEUTRALS:
            end = k
            while end < count and t[end] in _NEUTRALS:
                end += 1
            before = sos if k == 0 else strength(t[k - 1])
            after_kind = eos if end == count else strength(t[end])
            resolved = before if before is not None and before == after_kind else embedding
            for j in range(k, end):
                t[j] = resolved
            k = end
        else:
            k += 1

    # I1 and I2: implicit levels.
    for k, index in enumerate(sequence):
        kind = t[k]
        if level & 1:
            if kind in (BC_L, BC_EN, BC_AN):
                levels[index] = level + 1
            else:
                levels[index] = level
        else:
            if kind == BC_R:
                levels[index] = level + 1
            elif kind in (BC_AN, BC_EN):
                levels[index] = level + 2
            else:
                levels[index] = level
        types[index] = kind


def _resolve_brackets(sequence, t, classes, scalars, sos, embedding):
    """N0 over one isolating run sequence (BD16 bracket pairs)."""

    count = len(sequence)
    pairs: list[tuple[int, int]] = []
    stack: list[tuple[int, int]] = []  # (closing bracket, position)
    for k, index in enumerate(sequence):
        if t[k] != BC_ON:
            continue
        cp = scalars[index]
        cp = {0x2329: 0x3008, 0x232A: 0x3009}.get(cp, cp)
        pair, kind = bracket(cp)
        if kind == 1:
            if len(stack) == 63:
                break
            stack.append((pair, k))
        elif kind == 2:
            for depth in range(len(stack) - 1, -1, -1):
                if stack[depth][0] == cp:
                    pairs.append((stack[depth][1], k))
                    del stack[depth:]
                    break
    pairs.sort()

    def strong_of(kind):
        if kind == BC_L:
            return BC_L
        if kind in (BC_R, BC_AL, BC_EN, BC_AN):
            return BC_R
        return None

    for open_k, close_k in pairs:
        found_embedding = found_opposite = False
        for k in range(open_k + 1, close_k):
            strong = strong_of(t[k])
            if strong == embedding:
                found_embedding = True
                break
            if strong is not None:
                found_opposite = True
        if found_embedding:
            resolved = embedding
        elif found_opposite:
            context = sos
            for k in range(open_k - 1, -1, -1):
                strong = strong_of(t[k])
                if strong is not None:
                    context = strong
                    break
            resolved = context if context != embedding else embedding
        else:
            continue
        for k in (open_k, close_k):
            t[k] = resolved
            j = k + 1
            while j < count and classes[sequence[j]] == BC_NSM:
                t[j] = resolved
                j += 1


def visual_order(levels: list[int | None]) -> list[int]:
    """L2: logical indices in visual order; ``None`` levels are left out."""

    present = [(index, level) for index, level in enumerate(levels) if level is not None]
    if not present:
        return []
    order = [index for index, _ in present]
    values = [level for _, level in present]
    highest = max(values)
    lowest_odd = min((v for v in values if v & 1), default=highest + 1)
    for level in range(highest, lowest_odd - 1, -1):
        k = 0
        while k < len(order):
            if values[k] >= level:
                end = k
                while end < len(order) and values[end] >= level:
                    end += 1
                order[k:end] = order[k:end][::-1]
                values[k:end] = values[k:end][::-1]
                k = end
            else:
                k += 1
    return order


# ---------------------------------------------------------------------------
# Section 8: Arabic joining
# ---------------------------------------------------------------------------

def joining_forms(scalars: list[int]) -> list[int]:
    """Section 8: the form of each scalar in logical order."""

    types = [joining_type(props(cp)) for cp in scalars]
    forms = [FORM_ISOLATED] * len(scalars)
    for index, kind in enumerate(types):
        if kind not in (JT_D, JT_R, JT_L):
            continue
        prev = next((types[j] for j in range(index - 1, -1, -1) if types[j] != JT_T), JT_U)
        nxt = next((types[j] for j in range(index + 1, len(types)) if types[j] != JT_T), JT_U)
        joins_prev = kind in (JT_D, JT_R) and prev in (JT_D, JT_L, JT_C)
        joins_next = kind in (JT_D, JT_L) and nxt in (JT_D, JT_R, JT_C)
        if joins_prev and joins_next:
            forms[index] = FORM_MEDIAL
        elif joins_prev:
            forms[index] = FORM_FINAL
        elif joins_next:
            forms[index] = FORM_INITIAL
    return forms


# ---------------------------------------------------------------------------
# Section 6 and 9: one row on the cell grid
# ---------------------------------------------------------------------------

@dataclass(frozen=True, slots=True)
class PlacedCharacter:
    """One character of a row, where it sits and what it shows."""

    start: int          # first scalar offset in the logical row
    scalars: int        # number of scalars
    text: str           # display scalars: after mirroring and joining
    width: int          # 1 or 2 cells
    level: int          # resolved level of its first scalar
    column: int         # first cell, counted from the row's left edge


@dataclass(frozen=True, slots=True)
class RowLayout:
    """A logical row laid out left to right from visual column zero."""

    characters: tuple[PlacedCharacter, ...]   # visual order, width > 0
    width: int
    length: int                               # scalars in the logical row
    paragraph_level: int
    starts: tuple[int, ...]                   # every character start, logical

    @property
    def rtl(self) -> bool:
        return bool(self.paragraph_level & 1)

    def character_at_column(self, column: int) -> PlacedCharacter | None:
        for placed in self.characters:
            if placed.column <= column < placed.column + placed.width:
                return placed
        return None

    def position_at_column(self, column: int) -> int:
        """Section 9.1 for a visual column counted from the left edge.

        A column past the content on the paragraph's end side names the
        row's end; past it on the start side, the row's start.  The end side
        is the right of an LTR paragraph and the left of an RTL one.
        """

        placed = self.character_at_column(column)
        if placed is not None:
            return placed.start
        before = column < 0
        if before != self.rtl:
            return 0
        return self.length

    def character_starting_at(self, offset: int) -> PlacedCharacter | None:
        for placed in self.characters:
            if placed.start == offset:
                return placed
        return None

    def caret_character(self, offset: int) -> PlacedCharacter | None:
        """The visible character a caret at ``offset`` belongs to (Section
        9.2), or None at the row's end, where the caret sits past the content
        on the paragraph's end side."""

        if offset >= self.length:
            return None
        # An offset inside a character is shown at that character's start.
        start = max((s for s in self.starts if s <= offset), default=0)
        for candidate in sorted(s for s in self.starts if s >= start):
            placed = self.character_starting_at(candidate)
            if placed is not None:
                return placed
        return None


def layout_row(
    text: str | bytes,
    direction: int = DIRECTION_AUTO,
    *,
    keep_tab: bool = False,
) -> RowLayout:
    """Lay out one row of logical text as Sections 3 to 8 describe."""

    scalars = display_scalars(text, keep_tab=keep_tab)
    spans = segment(scalars)
    starts = tuple(start for start, _ in spans)
    if all(0x20 <= cp < 0x7F for cp in scalars) and direction != DIRECTION_RTL:
        placed = tuple(
            PlacedCharacter(i, 1, chr(cp), 1, 0, i) for i, cp in enumerate(scalars)
        )
        return RowLayout(placed, len(scalars), len(scalars), 0, starts)
    classes = [bidi_class(props(cp)) for cp in scalars]
    levels, paragraph = bidi_levels(classes, direction, scalars)
    forms = joining_forms(scalars)
    items = []
    for start, length in spans:
        cluster = scalars[start:start + length]
        width = char_width(cluster)
        if width == 0:
            continue
        level = levels[start]
        if level is None:
            level = next((lv for lv in levels[start:start + length] if lv is not None), paragraph)
        first = cluster[0]
        if level & 1:
            first = mirror(first)
        form = arabic_form(cluster[0], forms[start]) if first == cluster[0] else 0
        if form:
            first = form
        shown = "".join(map(chr, [first, *cluster[1:]]))
        items.append((start, length, shown, width, level))
    order = visual_order([item[4] for item in items])
    column = 0
    placed_list = []
    for index in order:
        start, length, shown, width, level = items[index]
        placed_list.append(PlacedCharacter(start, length, shown, width, level, column))
        column += width
    return RowLayout(tuple(placed_list), column, len(scalars), paragraph, starts)


@lru_cache(maxsize=4096)
def cached_row(text: str, direction: int = DIRECTION_AUTO, keep_tab: bool = False) -> RowLayout:
    return layout_row(text, direction, keep_tab=keep_tab)
