# APT-1 text rules

Contract ID: `APT-1-TEXT-1-2026-09-26`

Status: normative. This document is mirrored byte for byte in Akashic and
MegaPad. It is shared by the CELL-1 cell plane (`APT-1-WIRE.md`), RETAINED-1
`GLYPH_RUN` text and renderer-laid-out labels (`APT-1-RETAINED-1.md`), and the
STX1 semantic text body (MegaPad's `SEMANTIC-CONTENT-1.md`).

The key words MUST, MUST NOT, SHOULD, SHOULD NOT, and MAY are normative.

## 1. Scope

Both ends of a terminal session place text on the same cell grid, and each
must agree with the other about what one character is, how many cells it
takes, in what order characters appear, and which glyph form they take. This
document pins those rules once. It covers:

- wide characters (Chinese, Japanese, Korean, most emoji);
- characters built from several scalars: combining marks, emoji sequences
  with joiners, modifiers, and variation selectors, keycaps, and flags;
- right-to-left text in Hebrew and Arabic, including Arabic letter joining;
- the mapping between logical text positions and cells.

The selected renderer still owns fonts, glyph images, sizes, colors, and
pixels. No font name, font metric, or glyph identifier crosses the wire.

Out of scope for this contract: shaping of Indic and other complex scripts
beyond what these rules give, mirroring whole interfaces for right-to-left
users, vertical text, and line breaking. Rows are always explicit.

## 2. Unicode version and data

The rules use Unicode 15.1.0 and exactly these Unicode Character Database
files:

| File | Properties used |
| --- | --- |
| `UnicodeData.txt` | General_Category, decomposition tags of Arabic presentation forms |
| `EastAsianWidth.txt` | East_Asian_Width, with its `@missing` defaults |
| `HangulSyllableType.txt` | Hangul_Syllable_Type |
| `DerivedCoreProperties.txt` | Default_Ignorable_Code_Point, Indic_Conjunct_Break |
| `auxiliary/GraphemeBreakProperty.txt` | Grapheme_Cluster_Break |
| `emoji/emoji-data.txt` | Emoji, Emoji_Modifier, Extended_Pictographic |
| `extracted/DerivedBidiClass.txt` | Bidi_Class, with its `@missing` defaults |
| `BidiBrackets.txt` | Bidi_Paired_Bracket, Bidi_Paired_Bracket_Type |
| `BidiMirroring.txt` | Bidi_Mirroring_Glyph |
| `extracted/DerivedJoiningType.txt` | Joining_Type, with its defaults |

Implementations generate their tables from these files with a checked-in
generator. Tables are never edited by hand. Moving to a later Unicode version
is a change to this contract.

The conformance data is `auxiliary/GraphemeBreakTest.txt`, `BidiTest.txt`,
and `BidiCharacterTest.txt` of the same version.

## 3. Characters

A character is one extended grapheme cluster as defined by UAX #29 for
Unicode 15.1.0, rules GB1 to GB999, including GB9c. Text is always segmented
in logical order. A segmenter MUST pass every case in `GraphemeBreakTest.txt`.

Scalar offsets remain the unit of every wire position (Section 9). A position
names a boundary between scalars; the client places carets and selection
endpoints only on character boundaries.

## 4. Widths

The width `w(s)` of one scalar `s` is:

1. 0 if its General_Category is `Mn`, `Me`, or `Cf`, or its
   Hangul_Syllable_Type is `V` or `T`;
2. otherwise 2 if its East_Asian_Width is `W` or `F`;
3. otherwise 1.

Scalars excluded by Section 5 have no width; they never reach the grid.

The width `W(c)` of a character `c` whose scalars are `s0 s1 ... sn` is:

1. 0 if every scalar of `c` is Default_Ignorable_Code_Point;
2. otherwise 2 if `s0` and `s1` are both Regional_Indicator (a flag);
3. otherwise 2 if `s0` has Emoji=Yes and `s1` is U+FE0F VARIATION
   SELECTOR-16 or has Emoji_Modifier=Yes (emoji presentation);
4. otherwise 1 if `w(s0)` is 0 (a mark or joiner with no base before it);
5. otherwise `w(s0)`.

The width of a string is the sum of the widths of its characters. U+FE0E
does not narrow a character. A character of width 0 takes no cell and draws
nothing; it still occupies its scalar offsets in logical text.

## 5. Invalid input

Before segmentation, a producer replaces:

- each maximal ill-formed UTF-8 subsequence with U+FFFD;
- each scalar of General_Category `Cc`, `Zl`, or `Zp` with U+FFFD, except
  where its own context gives it a meaning, such as U+0009 in a text area or
  a line feed that separates rows.

U+FFFD has width 1. This is the only case in which text is forced to one
cell. Noncharacters, private-use, and unassigned scalars are valid and take
the width Section 4 gives them.

## 6. The cell grid

A displayed character occupies `W(c)` adjacent cells of one row: a lead cell
and, when the width is 2, one continuation cell to its right. A character
never spans rows.

When a clip edge would cut a two-cell character, it is not drawn; each of its
cells that is inside the clip shows a space in the character's style.

The glyph a cell shows is its character's display scalars: the character's
scalars in logical order, except that Section 7.4 may replace the first
scalar with its mirror and Section 8 may replace it with a joined Arabic form.
A renderer draws the display scalars as one glyph cluster fitted to the
character's one or two cells.

## 7. Right-to-left text

### 7.1 Paragraphs

Every row of text is one paragraph in the sense of UAX #9 BD2: a drawn label,
a text area row, a text grid item, a field. Its direction is one of:

- LTR (paragraph level 0);
- RTL (paragraph level 1);
- AUTO: the direction of the first strong character (UAX #9 rules P2 and
  P3, skipping isolate content), or LTR when there is none.

### 7.2 Levels

Levels follow UAX #9 for Unicode 15.1.0: rules X1 to X10 with the maximum
explicit depth of 125, W1 to W7, N0 to N2 with bracket pairs from
`BidiBrackets.txt`, I1 and I2, and L1. An implementation MUST produce the
levels and order required by every case in `BidiTest.txt` and
`BidiCharacterTest.txt`.

### 7.3 Visual order

On the grid, each character takes the resolved level of its first scalar.
Rule L2 reorders whole characters: the scalars inside one character stay in
logical order, so rule L3 does not apply. Characters that UAX #9 removes in
X9, and every character of width 0, take no cell.

### 7.4 Mirroring

A character at an odd level whose first scalar has a Bidi_Mirroring_Glyph
shows that glyph in place of its first scalar (UAX #9 rule L4). Characters
with Bidi_Mirrored=Yes and no mirroring glyph are drawn unchanged.

## 8. Arabic joining

Joining is decided per paragraph over its scalars in logical order, after
Section 5, using Joining_Type. Scalars of type `T` are skipped when looking
for neighbours. For a scalar `X`:

- `X` joins its previous neighbour when `X` is `D` or `R` and the nearest
  previous scalar that is not `T` is `D`, `L`, or `C`;
- `X` joins its next neighbour when `X` is `D` or `L` and the nearest next
  scalar that is not `T` is `D`, `R`, or `C`.

The form of `X` is medial when it joins both neighbours, final when it joins
only the previous one, initial when it joins only the next one, and isolated
otherwise.

A character whose first scalar has an Arabic Presentation Forms-A or -B
scalar for its form shows that scalar in its place. The form scalars are the
single-scalar decompositions tagged `<isolated>`, `<final>`, `<initial>`, and
`<medial>` in `UnicodeData.txt`, restricted to U+FB50..U+FDFF and
U+FE70..U+FEFF. When no form scalar exists for the needed form, the nominal
scalar is shown. Ligatures such as lam with alef are not formed; each letter
keeps its own cell.

## 9. Positions and the caret

Logical text is never reordered on the wire. Positions are scalar offsets
into a row's logical text.

### 9.1 From a point to a position

A point on the cells of a character names that character's starting offset,
the boundary logically before it. A point past the row's content on its end
side names the row's end.

### 9.2 Where the caret is shown

For a caret at offset `k`:

- when a character starts at `k`, the caret belongs to that character: a
  cell renderer marks its lead cell, and a renderer with finer positions
  draws it at the character's leading edge, which is its left edge at an even
  level and its right edge at an odd level;
- when `k` is the row's end, the caret is shown just past the row's content
  on its end side: to the right of it in an LTR paragraph and to the left of
  it in an RTL paragraph.

A selection covers exactly the characters whose logical offsets lie between
its two endpoints. In mixed-direction text its cells need not be contiguous.

## 10. Where these rules apply

- **CELL-1 cells** carry display scalars in visual order. Wide, continuation,
  and cluster cells follow `APT-1-WIRE.md` Section 11. The client does all
  segmentation, ordering, mirroring, and joining; the terminal only draws.
- **`GLYPH_RUN` text** is visual-order display text taken from cells. The
  renderer segments it by Section 3 and gives each character `W(c)` slots,
  left to right. It applies no reordering, mirroring, or joining.
- **Renderer-laid-out labels**, such as menu, tab, and shortcut text and
  READOUT units, are logical text. The renderer lays each one out as one AUTO
  paragraph using Sections 3, 7, and 8. Their widths are the renderer's,
  since such labels may use proportional fonts.
- **STX1 text** is logical text. `SEMANTIC-CONTENT-1.md` defines its
  paragraph direction, its columns in cells, and how rows are laid out.

## 11. Cost

Work added by these rules must keep a cheap path for plain left-to-right text.
A row whose scalars are all printable ASCII needs no table lookup: each byte
is one character of width 1 at level 0. A row with no scalar of Bidi_Class
`R`, `AL`, `AN`, `RLE`, `RLO`, `RLI`, or `FSI`, in an LTR or AUTO paragraph,
needs no bidi processing and no joining.
