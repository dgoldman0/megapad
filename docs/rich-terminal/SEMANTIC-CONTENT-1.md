# SEMANTIC-CONTENT-1 protocol slice

Status: protocol value, wire codec, immutable retained model, server ingress,
renderer-neutral immutable draw projection, and exact shared-viewer transport
implemented. The reference Pygame sink now rasterizes every collection kind and
publishes immutable TAB hit targets from the exact paint pass. The paired
Akashic `desktop-apt1` producer now advertises the capability, projects ordinary
UIDL/canonical-widget values, and has exercised all four kinds plus
acknowledgement-bound TAB activation through that sink. MegaPad's terminal
core, guest module, shared host, and reference viewer also implement the
positioned `PLACE`, `EXTEND`, `SCROLL`, and `FOLLOW` input specified below,
and the reference viewer draws style runs through its theme. A physical
renderer must not advertise `RET_CONTROL_COLLECTIONS` until its compositor
and acknowledgement path can render every visible kind.

The `ITEM_VIEW` kind, its `ITM1` body, and the item input below serve part 4
of Akashic's rich experience plan. MegaPad implements them: the ITM1 codec,
wire, scene, terminal core, view, reference renderer and hit map, viewer
routing, shared-viewer transport, and guest module. The paired Akashic
`desktop-apt1` producer publishes item views and advertises
`RET_CONTROL_ITEMS`. Wrapping card fields, which serve Akashic's Agent
transcript, are specified below: the `WRAP` column flag, line feeds in its
fields, exact card rows, and the viewport row. Nothing implements them yet.

## Decision

Text areas, logical text grids, tabsets, tabs, and item views extend the
existing retained `CONTROL` namespace. They do not create message families of
their own, an applet scene API, or terminal-buffer reservations.

The wire does not equate one CONTROL root with one UIDL source element. One
ordinary core UIDL type or canonical reusable widget may automatically project
multiple roots—such as a TABSET and a TEXT_AREA—using lower-layer control IDs
associated with its attachment, source index, and stable per-element object
key. Those producer coordinates do not enter the wire. Control IDs are exact
retained-graph identities, not permanent application identities: a complete
replacement may assign fresh IDs while the producer coordinates and value
signatures preserve semantic continuity. The results remain ordinary
independent CONTROL definitions in one owner and region, not a mirrored DOM or
element-owned scene. Applets do not register a provider or maintain a second
semantic description.

Feature bit 9, `RET_CONTROL_COLLECTIONS`, gates the text, grid, tabset, and
tab kinds and depends on bit 8 `RET_CONTROLS`. Feature bit 10,
`RET_CONTROL_ITEMS`, gates `ITEM_VIEW` and the item input below and depends
on bits 8 and 9. The same `CONTROL_DEFINE`, `CONTROL_REPLACE`,
`CONTROL_DROP`, owner authority, independent control-ID high-water, object
quota, UTF-8 quota, transaction, and exact-revision publication rules apply.
Menus remain valid with bit 8 alone.

This is one extensible semantic record with two content bodies, the text
collection `STX1` and the item collection `ITM1`:

| Control kind | Value | Shape |
|---|---:|---|
| `TEXT_AREA` | 5 | bounded root plus one `STX1` logical text collection |
| `TEXT_GRID` | 6 | bounded root plus one `STX1` logical text collection |
| `TABSET` | 7 | bounded root, no content body |
| `TAB` | 8 | renderer-laid-out `TABSET` child using the existing label/shortcut fields |
| `ITEM_VIEW` | 9 | bounded root plus one `ITM1` item collection |

The design is renderer-neutral. It carries logical rows, columns, spans,
stable item keys, logical-order text with a paragraph direction, style runs
that say what parts of the text mean, a generic viewport origin,
authoritative state, and selection/caret positions. Item views carry each
item's key, parent, depth, fields, and state, and a viewport over the items'
order, and say whether they are a list, a tree, a table, sections, or cards.
It does not carry a
retained-cell capacity, font, colour, text size, padding, pixel rectangle,
refresh waveform, e-paper cadence, or physical hit box. Characters, widths,
ordering, mirroring, and joining follow the shared text rules in
`APT-1-TEXT.md`.

## CONTROL envelope

The exact 80-byte prefix is `<QQQHHiQQIiiIIIII>`. Its optional root bounds use
the common signed-cell-origin/unsigned-positive-extent contract, with all-zero
as canonical absence. Its last three u32 fields are `label_bytes`,
`shortcut_bytes`, and `content_bytes`. The exact payload is:

```
80-byte CONTROL prefix
label_bytes bytes of clean UTF-8
shortcut_bytes bytes of clean UTF-8
content_bytes bytes of canonical semantic content
```

Menu controls, `TABSET`, and `TAB` require `content_bytes = 0`.
`TEXT_AREA` and `TEXT_GRID` require a nonempty canonical `STX1` body. The
smallest body is 72 bytes, so advertising bit 9 requires at least 152 inbound
payload bytes and a retained transaction maximum of at least 352 bytes.
`ITEM_VIEW` requires a nonempty canonical `ITM1` body, whose smallest form is
56 bytes; bit 10 depends on bit 9, whose minima already cover it. There
is no second item-count or content-byte policy maximum; the negotiated frame
maximum, retained transaction maximum, object quota, owner aggregate UTF-8
quota, and caller's terminal allocation are the bounds.
Every CONTROL record consumes one existing object-quota slot and every STX1
or ITM1 item consumes one more; this accounts for stable retained values a
selected renderer may materialize without introducing a new kind-specific
capacity.

Existing menu frames are byte-for-byte unchanged because their former zero
reserved field is still zero as `content_bytes`. This repository is unreleased,
so no decoder keeps the rejected intermediate interpretation that required the
field to be reserved forever. Capability bit 9 prevents an older terminal from
being sent a new kind or body. The current Akashic driver deliberately rejects
unknown advertised bits. The synchronized driver now understands bit 9, while a
bit-8-only policy remains the exact menu-compatible path for a terminal that
does not advertise collections.

## STX1 body

All integers are little-endian. The 72-byte header is
`<IHHQIIIIIIIIQQII>`:

| Offset | Field | Type |
|---:|---|---|
| 0 | tag = `0x31585453` (`STX1`) | u32 |
| 4 | version = 1 | u16 |
| 6 | reserved = 0 | u16 |
| 8 | content revision | u64, positive |
| 16 | logical document/grid rows | u32, positive |
| 20 | logical document/grid columns | u32, positive |
| 24 | viewport row origin | u32, less than rows |
| 28 | viewport column origin | u32, less than columns |
| 32 | viewport row extent | u32, positive and in bounds |
| 36 | viewport column extent | u32, positive and in bounds |
| 40 | item count | u32 |
| 44 | content flags | u32 |
| 48 | primary item key, zero when absent | u64 |
| 56 | selection-anchor item key, zero when absent | u64 |
| 64 | primary Unicode-scalar offset | u32 |
| 68 | anchor Unicode-scalar offset | u32 |

Content flag bit 0 is `READ_ONLY`. Bits 1 and 2 hold the paragraph
direction of every row or item (`APT-1-TEXT.md` Section 7.1): 0 `AUTO`,
1 `LTR`, 2 `RTL`; 3 is invalid. All other bits are zero.

Exactly `item_count` variable records follow. Each begins with the 36-byte
header `<QIIIIHHII>`, followed immediately by its UTF-8 text and then its
style runs:

| Offset | Field | Type |
|---:|---|---|
| 0 | stable nonzero item key | u64 |
| 8 | logical row | u32 |
| 12 | logical column | u32 |
| 16 | positive row span | u32 |
| 20 | positive column span | u32 |
| 24 | role | u16 |
| 26 | state | u16 |
| 28 | text bytes | u32 |
| 32 | style run count | u32 |

Each style run is the 12-byte record `<IIHH>`: u32 start, a Unicode-scalar
offset into the item's text; u32 length in scalars, positive; u16 meaning;
and u16 reserved = 0. The section on style runs below gives their rules.

Roles are 1 `CONTENT`, 2 `ROW_HEADER`, and 3 `COLUMN_HEADER`. State bit 0 is
`CURRENT`; bit 1 is `UNAVAILABLE`; other bits are zero and an unavailable item
cannot be current. Text is well-formed Unicode scalar UTF-8 in logical order
and contains no C0 control scalar other than U+0009 HORIZONTAL TAB, and no
DEL. A tab is one character one cell wide. Offsets count Unicode scalar
values, not UTF-8 bytes or characters. The client places them on character
boundaries; a renderer treats an offset inside a character as that
character's start.

Rows and columns on every item are absolute document/grid coordinates. The
origin and positive extents define the exact half-open logical viewport
rectangle. The selected renderer maps only that rectangle into the root bounds
and must not expose additional logical rows or columns merely because its font
leaves spare pixels. For TEXT_AREA the column coordinates count cells: a row's
width is the sum of its characters' widths (`APT-1-TEXT.md` Section 4). For
TEXT_GRID they are logical grid columns.

The producer carries every source semantic item intersecting that rectangle;
an omitted coordinate inside it asserts empty/absent content. Items wholly
outside may be omitted. A primary or anchor endpoint outside the rectangle
remains carried so its key and scalar offset stay authoritative; the renderer
clips that item rather than drawing it at the viewport origin.

Records are in `(row, column, item_key)` order, keys are unique, rectangles fit
the declared logical dimensions, and all half-open item rectangles are
pairwise nonoverlapping in two dimensions. Thus a row-spanning item does not
exclude a later item in other columns. A nonzero primary or anchor key names a
carried item and its offset is within that item's scalar length. An anchor
requires a primary. Trailing bytes, impossible header counts, unknown
versions/roles, reserved bits, and noncanonical geometry are rejected.

`TEXT_AREA` restricts every item to a `CONTENT` value with `state = 0` spanning one
complete logical document row. Sparse rows outside the carried viewport are
ordinary omission; a missing row inside the explicit viewport renders empty. A
carried row is at most `columns` cells wide. Primary and anchor name the caret
and optional selection endpoint.

Each TEXT_AREA row is one paragraph with the content's direction. Its
characters take their cells in visual order (`APT-1-TEXT.md` Sections 6 to
8). A row whose paragraph resolves to LTR starts at the viewport's left edge,
and the column origin counts cells from the left. A row that resolves to RTL
is mirrored: it starts at the viewport's right edge, and the column origin
counts cells from the right. Horizontal scrolling therefore moves both kinds
of row away from their own start edge. Carets and selections are shown as
`APT-1-TEXT.md` Section 9.2 says.

Each TEXT_GRID item's text is one paragraph with the content's direction,
ordered, mirrored, and joined in the same way. Where it sits inside the
item's rectangle is the renderer's choice. `TEXT_GRID`
permits all three roles and rectangle spans; its positions name whole items and
therefore use zero offsets and no anchor. At most one `CURRENT` grid item
exists. The primary item is the authoritative selection and may differ from
`CURRENT` (for example, a selected calendar date distinct from today).

### Style runs

Style runs say what parts of an item's text mean, so that each renderer can
choose how they look. A run names a start, a length, and one meaning from
this list:

| Value | Meaning | Marks |
|---:|---|---|
| 1 | `KEYWORD` | a keyword of a programming language |
| 2 | `COMMENT` | a comment |
| 3 | `STRING` | a string or character literal |
| 4 | `NUMBER` | a numeric literal |
| 5 | `HEADING` | a heading |
| 6 | `EMPHASIS` | emphasised text |
| 7 | `STRONG` | strongly emphasised text |
| 8 | `CODE` | code set within prose |
| 9 | `LINK` | a link the reader can follow |
| 10 | `ERROR` | text the application reports as wrong |

Runs are in increasing start order, lie within the item's text, and do not
overlap. Two runs with the same meaning never touch: the client writes them
as one run. Text that no run covers is plain. Only `TEXT_AREA` items carry
runs in this version; a `TEXT_GRID` item has zero runs. A zero length, a
meaning not in the list, a nonzero reserved field, and runs out of order,
overlapping, touching with the same meaning, or reaching past the text are
rejected.

A character takes the meaning of the run that covers its first scalar, so a
run boundary inside a character never splits it. The client need not place
run boundaries on character boundaries.

Runs carry no colour, font, or size. A renderer shows each meaning in a look
its own theme chooses, such as a colour, a weight, a slant, or an underline,
and a meaning never moves a character from the cells the text rules give it.
A heading therefore keeps its cells: it is not set in larger text. A theme
may show two meanings alike, or show a meaning like plain text, except
`LINK`: a link always looks different from plain text, so that a reader can
find it. An e-paper theme might use weight and underline where a colour
screen uses colour.

## ITM1 body

An item view shows a collection of items as a list, a tree, a table,
sections, or cards. The client says which, and gives each item a stable key,
its fields, and its state. The renderer lays the items out and never infers
structure from their text or indentation.

All integers are little-endian. The 48-byte header is `<IHHQHHIIIIIII>`:

| Offset | Field | Type |
|---:|---|---|
| 0 | tag = `0x314D5449` (`ITM1`) | u32 |
| 4 | version = 1 | u16 |
| 6 | reserved = 0 | u16 |
| 8 | content revision | u64, positive |
| 16 | role | u16 |
| 18 | content flags | u16 |
| 20 | column count | u32, positive |
| 24 | item total | u32 |
| 28 | viewport first | u32 |
| 32 | viewport count | u32 |
| 36 | carried item count | u32 |
| 40 | viewport row | u32 |
| 44 | reserved = 0 | u32 |

Roles are 1 `LIST`, 2 `TREE`, 3 `TABLE`, 4 `SECTIONS`, and 5 `CARDS`.
Content flag bits 0 and 1 hold the paragraph direction of every field and
column label (`APT-1-TEXT.md` Section 7.1): 0 `AUTO`, 1 `LTR`, 2 `RTL`; 3 is
invalid. All other bits are zero.

Exactly `column count` column records follow the header. Each is the 8-byte
`<HHI>`: u16 column kind, u16 column flags, and u32 label bytes, followed by
the label. Column kinds are 1 `TEXT` and 2 `NUMBER`. A `NUMBER` column holds
quantities such as sizes or counts. A label names its column and may be
empty. Column flag bit 0 is `WRAP`: the column's fields break into lines, as
the rendering rules below say. Only a `CARDS` view may have a `WRAP` column.
Other flag bits are zero.

Exactly `carried item count` items follow the columns. Each begins with the
32-byte header `<QQIHHHHI>`:

| Offset | Field | Type |
|---:|---|---|
| 0 | stable nonzero item key | u64 |
| 8 | parent item key, zero when none | u64 |
| 16 | ordinal | u32 |
| 20 | depth | u16 |
| 22 | state | u16 |
| 24 | item role | u16 |
| 26 | field count | u16 |
| 28 | reserved = 0 | u32 |

Then come `field count` fields, one per column from the first. Each field is
the 8-byte `<II>` (u32 text bytes, u32 style run count), then its text, then
its style runs, exactly as an STX1 item carries its text and runs. Every
item has at least one field and no more fields than columns.

Item roles are 1 `ITEM` and 2 `SECTION`, a heading that starts a section.
Item state bits are:

| Bit | Name | Meaning |
|---:|---|---|
| 0 | `SELECTED` | the application's selection |
| 1 | `CURRENT` | the item the application marks as current, such as the open folder |
| 2 | `EXPANDABLE` | the item has children that can be shown |
| 3 | `EXPANDED` | its children are shown |
| 4 | `CHECKABLE` | the item has a check box |
| 5 | `CHECKED` | the check box is checked |
| 6 | `UNAVAILABLE` | the item cannot be selected, opened, or checked |

Other bits are zero. `EXPANDED` requires `EXPANDABLE`, `CHECKED` requires
`CHECKABLE`, and an `UNAVAILABLE` item is not `SELECTED`. At most one item
is `SELECTED` and at most one is `CURRENT`. A `SECTION` has state zero and
exactly one field.

Items have the order the application shows them in: ordinals count from
zero to `item total` minus one. The viewport is the half-open range of
ordinals from `viewport first` for `viewport count` items. When the total is
zero, both are zero. Otherwise `viewport first` is less than the total, and
the count is positive and reaches no further than the total. Every ordinal in
the viewport is carried. Items outside it may be carried or omitted, but a
`SELECTED` item is always carried, so its key stays authoritative. Carried
items are in increasing ordinal order, and their keys are unique.

The viewport row is zero except in `CARDS`, where it counts the rows of the
first viewport item that lie above the root's top edge, so a view can scroll
through a card taller than the root. It is less than that item's row count
at the control's width, as the rendering rules below give it, so the first
viewport item always shows a row. A terminal checks it against the control's
bounds before it commits the control.

The role fixes the structure:

- In a `LIST`, `TABLE`, or `CARDS` view, every item is an `ITEM` with
  parent zero and depth zero, and none is `EXPANDABLE`. No `CARDS` item is
  `CHECKABLE`.
- In a `TREE`, every item is an `ITEM`. A top-level item has parent zero and
  depth zero. Any other item names its parent and has depth one more than
  the parent's. A carried parent has a lower ordinal and is `EXPANDABLE`
  and `EXPANDED`.
- In `SECTIONS`, each `SECTION` has parent zero and depth zero, and each
  `ITEM` has depth one and names its section as parent. A carried parent is
  a `SECTION` with a lower ordinal. No item is `EXPANDABLE`.

Ordinals follow the structure in preorder. Between two carried items with
consecutive ordinals the depth rises by at most one, and when it rises, the
second item's parent is the first.

Field text and labels are well-formed Unicode scalar UTF-8 with no C0
control scalar and no DEL, except that a field in a `WRAP` column may contain
U+000A LINE FEED, which ends a paragraph. Such a field is one paragraph more
than it has line feeds; every other field, and every label, is one paragraph.
Each paragraph has the content's direction and is laid out by the shared text
rules. Fields may carry style runs, with the rules and meanings of STX1's
style runs; a line feed is a scalar that a run may cover. This version
defines no way to follow a link in a field. Trailing bytes, impossible
counts, unknown versions, roles, kinds, or state bits, nonzero reserved
fields, and any rule above that fails are rejected.

The renderer shows the viewport's items in order within the root bounds,
and never an item outside the viewport, even when space remains; items that
do not fit are clipped. It chooses the fonts, metrics, and indentation, the
disclosure mark of an `EXPANDABLE` item, the check box of a `CHECKABLE` one,
and how `SELECTED`, `CURRENT`, and `UNAVAILABLE` look. A `TABLE` shows its
column labels as a header when any is nonempty. A `NUMBER` field may be
aligned at its column's end.

`CARDS` shows each item's fields together as one card, and its rows are
exact, so that client and renderer agree on every row. With `W` the root's
width in cells, field 0's lines are at most `max(W - 2, 1)` cells wide and
each later field's at most `max(W - 4, 1)`. A card's rows are its fields'
lines, column by column:

- a field in a column without `WRAP` is one line, clipped at its width;
- a field in a `WRAP` column is broken into lines of its width, paragraph by
  paragraph, by `APT-1-TEXT.md` Section 12; an empty paragraph is one line;
- a column the item has no field for is one empty line.

The first viewport item starts `viewport row` rows above the root's top
edge, each later item starts on the row after the one before it ends, and
rows past the root's bottom edge are clipped. The renderer adds no row
between cards and chooses where each field's lines sit across the card,
within their widths. A card box or other mark lies within the card's rows.

## Hierarchy and mutation

`TEXT_AREA`, `TEXT_GRID`, `TABSET`, and `ITEM_VIEW` are bounded roots with
parent and order zero. They have no label or shortcut. `TAB` is a
label-bearing child of one
same-owner, same-region `TABSET`; it has renderer-owned child geometry and a
unique sibling order. At most one visible/enabled tab is `SELECTED` per tabset.
TEXT_AREA, TEXT_GRID, ITEM_VIEW, and TAB admit `VISIBLE`, `ENABLED`, and
`SELECTED`; TABSET admits `VISIBLE` and `ENABLED`. As elsewhere, `SELECTED`
requires the same control to be visible and enabled.

Menu and tabset replacements retain the existing state-only rule. `TAB`
replacement may change state, label, or shortcut while preserving identity and
hierarchy. Text area, text grid, and item view replacement may change state
and the complete semantic content while preserving identity and geometry; a
changed body must carry a strictly newer content revision. UTF-8 usage is
removed and added atomically against the owner's existing aggregate
reservation.

`CONTROL_EVENT` activation is sufficient for `TAB` and remains revision-bound.
Existing revision-bound KEY/TEXT input remains usable by the authoritative
focused UI. Pointer input on text areas and grids uses the positioned kinds
below, and on item views the item kinds.

## Positioned input

`CONTROL_EVENT` kinds 2 to 5 let a pointer act on text areas and grids without
the terminal guessing application state. They require feature bit 9. Their
position tail names the exact acknowledged content: `content_revision` is the
control's STX1 content revision in the composite named by `model_revision`,
`item_key` names an item carried in that content, and `scalar_offset` is a
Unicode-scalar boundary within that item. The terminal computes a position
from its own presentation of the content and emits an event only for a
visible, effectively enabled root. `READ_ONLY` content admits all four kinds,
because they move the caret, selection, or viewport, or follow a link, and
never change the text.

`PLACE` puts the caret in a text area or selects a grid item:

- On a TEXT_AREA, a point on a carried row names that row and the position
  `APT-1-TEXT.md` Section 9.1 gives in the terminal's layout of the row: the
  start of the character drawn under the point, or the row's end for a point
  past the row's content on its end side. A point on a viewport row with no
  carried item names the nearest carried row above it at its scalar length
  or, when none is above, the nearest carried row below it at offset zero.
  With no carried row in the viewport the terminal emits nothing.
- On a TEXT_GRID, the point names the item whose rectangle contains it, with
  offset zero. Only a `CONTENT` item without `UNAVAILABLE` may be named; any
  other point emits nothing.

`EXTEND` names a TEXT_AREA position exactly as `PLACE` does and moves the caret
there while keeping the selection anchor; when no selection exists, the prior
caret becomes the anchor. A terminal sends it for a press with Shift held and
for motion with the primary button held after a `PLACE` on the same root. It
needs no preceding `PLACE`. During such motion a point outside the root is
first clamped to the root's nearest edge.

`SCROLL` carries wheel detents over a TEXT_AREA or TEXT_GRID root. The client
decides how far one detent moves the viewport and whether the caret follows.

`FOLLOW` asks the client to follow a link in a TEXT_AREA. It names a
position as `PLACE` does, and only the start of a character whose meaning is
`LINK` may be named. A terminal sends it, instead of `PLACE`, for a primary
press on such a character with Ctrl held, and also for a press there
without Shift or Ctrl when the content is `READ_ONLY`. No `EXTEND` follows
that press. So in editable text a plain press on a link still places the
caret, and Ctrl and a press follows the link; in read-only text a plain press
follows it. The client decides what following a link does, such as opening
the file it names. The terminal never opens anything itself, and no link
target is carried on the wire.

The client revalidates the owner, generation, control identity, kind, event
revision, and content revision, and resolves the item key in the content it
published. It then routes the position to the ordinary widget that produced
that content, which applies it to its current state and clamps it if that
state has since moved on. A stale or unknown position is discarded, and so
is a `FOLLOW` whose position no longer lies on a link. The terminal never
changes the caret, selection, or viewport itself; the client publishes the
result in a later transaction.

## Item input

`CONTROL_EVENT` kinds 6 to 10 let a pointer act on the items of an item
view. They require feature bit 10. Each ends with the item tail `<QQII>`:
`content_revision`, the control's ITM1 content revision in the composite
named by `model_revision`; `item_key`, an item carried in that content and
shown in its viewport; and two u32 reserved fields that are zero. The
terminal emits them only for a visible, effectively enabled `ITEM_VIEW`
root.

- `SELECT` (6) asks the client to select the item. A terminal sends it for
  a primary press on an `ITEM` that is not `UNAVAILABLE`, other than on its
  disclosure mark or check box. The modifiers are carried, and the client
  decides what they mean.
- `OPEN` (7) asks the client to open the item, as a double click does. A
  terminal sends it, instead of `SELECT`, for a second primary press on the
  same item within its own double-press interval. The item is an `ITEM`
  that is not `UNAVAILABLE`.
- `EXPAND` (8) and `COLLAPSE` (9) ask the client to show or hide the
  children of an `EXPANDABLE` item. A terminal sends `EXPAND` for a primary
  press on the disclosure mark of an item that is not `EXPANDED`, and
  `COLLAPSE` for one that is.
- `CHECK` (10) asks the client to check or uncheck a `CHECKABLE` item that
  is not `UNAVAILABLE`. A terminal sends it for a primary press on the
  item's check box.

`SCROLL` (4) also targets `ITEM_VIEW` roots, with the same detents; the
client decides how far one detent moves the viewport.

The client revalidates the owner, generation, control identity, kind, event
revision, and content revision, and resolves the item key in the content it
published. It then routes the event to the ordinary widget that produced that
content, which applies it to its current state if the item is still there,
and otherwise discards it. The terminal changes no selection, expansion,
check, or viewport itself; the client publishes the result in a later
transaction. Keys such as the arrows and Enter still reach the focused
application as `KEY` input.

## Cost boundary

STX1 adds one 72-byte collection header, one 36-byte header per carried
text item, and 12 bytes per style run. Decode and canonical validation use
linear scalar/key/run passes. The
common one-row-span case uses a linear overlap pass; genuine row spans use an
`O(n log n)` rectangle sweep. They do not compute content hashes, rasterize,
scan terminal cells, or rebuild a second scene. Immutable values cache their
validated UTF-8 and wire byte totals, so quota admission and scene freezing do
not re-encode every string. That same canonical item loop derives exactly
three non-semantic summaries: whether the content has TEXT_AREA shape, how
many items carry `CURRENT`, and how many style runs it has. Later scene,
view, and shared-wire family checks consult
those immutable facts in `O(1)` instead of rescanning items. They add no hash,
certificate, cache, traversal, or wire field; canonical STX1 construction and
decode remain the boundary that proves the facts. Wire encoding still makes
one necessary UTF-8/body pass. Each item uses one existing object-quota slot,
so one accepted control replaces many per-row GLYPH_RUN definitions without
evading the caller's retained-value bound. `CONTROL_REPLACE` currently resends
the complete small collection.

ITM1 adds a 48-byte header, 8 bytes and the label per column, a 32-byte
header per carried item, and per field 8 bytes, its text, and 12 bytes per
style run. Decode and validation make one linear pass over columns, items,
fields, and runs, plus the key-uniqueness check, which may sort. They do not
walk or rebuild a tree: the preorder rules compare each carried item only
with the one before it and, through a key lookup, with its parent. Breaking a
`WRAP` field into lines is one pass over its text, and checking the viewport
row breaks only the first viewport item's fields.

That full replacement is the bounded first slice, not a claim that it is the
best steady-state Pad keystroke transport. Before adding machinery, measure its
guest instructions and exact UART bytes against the residual-glyph path. If
the complete visible text area becomes the bottleneck, the next protocol work
is one generic revision-bound STX1 item patch operation with atomic model
application—not Pad-specific events, a grid-only message family, hashes, or a
renderer cache exposed on the wire.

## Shared-viewer transport

The local JSON display-offer wire carries the renderer-facing values with exact
tags `text_area`, `text_grid`, `tabset`, nested `tab`, and `item_view`. Text
collection roots use the exact fields `kind`, `control_id`, `state`, `order`,
`z_order`, `bounds`, and `content_stx1_base64`. An item view has the same
fields with `content_itm1_base64`, canonical padded base64 of its ITM1 bytes,
decoded and checked as STX1 is below. A tabset replaces the content field with
`tabs`; each tab has only `kind`, `control_id`, `state`, `order`, `label`, and
`shortcut`.

`content_stx1_base64` is canonical padded base64 of the existing STX1 byte
value. The shared wire deliberately does not restate STX1 items as a second
JSON schema. Decode first requires canonical base64, then delegates all item,
UTF-8, geometry, selection, and reserved-field checks to
`decode_semantic_text_content`, and finally reasserts the TEXT_AREA or TEXT_GRID
family shape through the common retained-control validator. That reassertion is
constant-time: it reads the two immutable summaries already derived during
canonical STX1 content construction, rather than repeating the item scan on
both outgoing and incoming offers. Unknown or extra JSON fields, mismatched
tags, malformed STX1, ambiguous tab state, duplicate identities, and
noncanonical ordering fail closed before a display offer enters the viewer.

## Reference Pygame renderer

The reference sink paints the mandatory complete CELL image first, paints every
retained draw in deterministic back-to-front order, and leaves the existing
cursor overlay last. Collection roots are opaque rich representations over
that complete fallback; pixels outside their root bounds remain the CELL image.

TEXT_AREA maps the exact declared logical viewport to half-open integer row and
cell-column slots. Each row is laid out by `APT-1-TEXT.md` Sections 3 to 8 in
the content's direction: a character's display scalars are one glyph cluster
starting at its first slot, a wide character spans two slots, a right-to-left
row is mirrored, and a character the viewport edge cuts is not drawn. Missing
rows remain blank, U+0009 is a renderer-owned blank slot in this first policy,
the characters between the anchor and the primary receive a selection fill,
which need not be contiguous, and the primary position receives a persistent
caret at its character's leading edge, or past the row's content on its end
side (Section 9.2). An offscreen endpoint remains authoritative but is never
moved to the viewport origin. The reference sink uses the terminal monospace
font for this editor policy.

Each text area character takes the look the viewer's theme gives its
meaning. The reference theme gives every meaning its own colour, sets
`KEYWORD`, `HEADING`, and `STRONG` in the monospace family's bold face and
`EMPHASIS` and `COMMENT` in its italic face, underlines `LINK`, and draws a
red underline under `ERROR`. It uses the family's real bold and italic
faces when the host has them, and only otherwise synthesizes them from the
regular face. A style never changes a character's slots.

TEXT_GRID maps item rectangles directly from their logical viewport-relative
row, column, and span values. It paints role, primary, `CURRENT`, and
`UNAVAILABLE` states with renderer-owned styling and never materializes a
rows-by-columns matrix. Each item's text is laid out as one paragraph in the
content's direction, and a right-to-left item is set against its rectangle's
right edge. Tab, menu, and shortcut labels are laid out as AUTO paragraphs
(`APT-1-TEXT.md` Section 10). TABSET uses renderer-owned sans-serif metrics: natural
tab widths when they fit and deterministic equal partitioning when they do not.
Only physically visible, effectively enabled TAB children enter the immutable
hit map as activation targets. An enabled TEXT_AREA or TEXT_GRID root enters it
as a text target that keeps the exact partition and layout its paint pass
used, so a point maps to the item and position drawn there: on a text area
row, the start of the character on that slot, and past the content the row's
end on its end side and its start on the other (`APT-1-TEXT.md` Section 9.1). A disabled text root, and every menu bar, tabset, and
open popup, enters as a control surface: it blocks lower controls and never
starts a raw pointer gesture. A point covered only by a region barrier, or by
nothing, shows CELL or residual content and may start one.

An ITEM_VIEW shows its items in viewport order in the terminal monospace
font, one row each except in cards. A tree indents each item by its depth and
puts a disclosure mark before an `EXPANDABLE` one; a checkable item gets a
check box before its first field; a table puts its column labels in a header
row and aligns
`NUMBER` fields at their column's end; sections show each `SECTION` as a
heading; and cards give each item a box holding its fields' lines on the
exact rows the rules above give, with later fields indented two cells.
`SELECTED` items get a selection fill, `CURRENT` a mark, and `UNAVAILABLE`
dimmed text, and fields take their style runs' looks from the theme. An
enabled item view enters the hit map as an item target that keeps each shown
row's item and the rectangles of its disclosure mark and check box. The
reference viewer routes a left press there as `EXPAND`, `COLLAPSE`, `CHECK`,
or `SELECT` by what it hits, a second press on the same item within its
double-press interval as `OPEN`, and wheel input as `SCROLL`.

The reference viewer routes a left press on a text target as `PLACE` (as
`EXTEND` with Shift on a text area), a drag that began there as `EXTEND` at the
clamped position, and wheel input there as `SCROLL`. A left press on a text
area character whose meaning is `LINK` is `FOLLOW` instead when Ctrl is held,
or when the content is `READ_ONLY` and neither Shift nor Ctrl is; the text
target keeps each row's style runs for that test. Presses, drags, releases,
and wheel steps on residual content become raw `POINTER` input at the cell
under the pointer, with a release that cannot yet be sent kept until the
acknowledged display is current.

Raster code consumes the already validated immutable draw/content values. It
does not encode or decode STX1, rerun family/UTF-8/overlap proofs, or render an
unbounded whole collection string into one temporary surface. Text area, grid,
and tab text uses one glyph surface per character and emits only glyph pixels
that intersect the physical clip. That bounds each raster allocation and render call; it does not
claim bounded traversal of a long proportional-font prefix. Grid edges are
intersected as Python integers before constructing SDL-backed rectangles, so
valid extreme-u32 spans cannot wrap into false geometry. A paint failure occurs
before flip, hit-map staging, or physical acknowledgement.

The completed pixel surface—not STX1 revision, item geometry, or semantic
state—is the input to the opt-in panel-neutral damage tracker. A damage-aware
sink captures it only after CELL, rich planes, and cursor composition; the SDL
reference sink avoids that readback because it has no partial-refresh consumer.
The damage baseline advances only on exact sink acknowledgement. Partial/full
refresh, waveform, ghosting, color conversion, controller completion, and
settling remain selected-sink policy.

## Implementation boundary

The coherent protocol slice is owned by:

- `rich_terminal/semantic_content.py`: immutable STX1 values and exact codec;
- `rich_terminal/semantic_items.py`: immutable ITM1 values and exact codec;
- `rich_terminal/retained_model.py`: negotiated bit and caller-bound policy;
- `rich_terminal/retained_wire.py`: CONTROL envelope and kind validation;
- `rich_terminal/retained_scene.py`: authority, graph, quota, replacement, and
  content-revision rules;
- `rich_terminal/server.py`: normal PRESENT ingress; and
- `rich_terminal/retained_view.py`: immutable `TextAreaDraw`, `TextGridDraw`,
  and `TabSetDraw`/`TabDraw` values, exact active owner/region scope, canonical
  control-shape validation, and deterministic projection of independent
  sibling roots. The view reuses the deeply immutable STX1 content value
  validated at wire/model admission; it does not rebuild the item graph or
  repeat item-family, UTF-8, or rectangle-overlap scans on every display offer;
- `shared_session.py`: exact tagged local-viewer transport, carrying canonical
  STX1 bytes without defining a parallel item representation;
- `rich_terminal/pygame_view.py` and `session_viewer.py`: generic collection
  rasterization, same-pass immutable TAB hit geometry, complete CELL/cursor
  composition order, an explicit damage-sink capture helper, and synchronous
  SDL reference-sink promotion after successful flip. CELL skips a cell whose
  whole box an opaque GLYPH_RUN fill later repaints, unless its glyph
  overhangs the cell, and undecorated glyph runs blit each glyph cropped to
  its slot in one batch; `tests/test_rich_terminal_compositor_replay.py`
  holds both to the per-cell, per-slot reference pixel for pixel; and
- `rich_terminal/final_raster.py`: sink-local final-pixel damage, pinned
  raster/damage/hit-map offers, and acknowledgement-only baseline promotion.

The reference sink no longer blocks collection rendering. The synchronized
Akashic lower-layer producer now advertises bit 9 in `desktop-apt1`, and ordinary
core UIDL/canonical widgets emit these exact records through the CONTROL
encoder. The `4b6a475`/`29bdfd6` physical reference-view journey exercised two
text areas, one text grid, two tabsets, their tabs, and activation of Pad's
original tab after the Daybook-to-Pad handoff. Pad, Daybook, and every other
applet remain ordinary UIDL/TUI clients; none receives a provider callback,
terminal API, renderer-specific annotation, or future per-applet repair
obligation.

Advertisement was deliberately treated as one final vertical gate, not an
isolated policy bit flip. The MegaPad guest module accepts mask `0x73f`, requires bit 8 for
bit 9 and bit 9 for bit 10, and evolves the one public CONTROL writer to copy
caller-bounded kinds 5 through 9 without a parallel message or legacy encoder;
ITEM_VIEW requires bit 10 and at least the 56-byte smallest ITM1 body. It enforces exact root,
child, state, label, shortcut, and zero/nonzero content shapes; TEXT_AREA and
TEXT_GRID also reject a body shorter than the fixed 72-byte STX1 header. The
guest does not repeat canonical STX1 item/graph validation: Akashic supplies
those bytes, and the terminal validates them before commit. The three source
spans are checked for nonwrapping storage, staging/session aliasing, and mutual
overlap, then all source borrows are scrubbed after the guarded call.

Bit-9 discovery now audits the 152-byte peer payload, 192-byte guest TX staging,
and 352-byte retained-transaction minima, and a focused target-Forth oracle
locks the complete minimum-content CONTROL frame. No generic MegaPad default
advertises the capability implicitly; the explicit selected Akashic
`desktop-apt1` policy does. Its real core-owned collection operation counts and
UTF-8 bytes are included in the Desktop caller-owned arena derivation, and its
ordinary core-widget projection/encoder drives the accepted records.

The session configuration derives its transport `max_payload` from both the CELL
row requirement and the retained client-to-terminal payload policy. This keeps
an otherwise valid retained policy from failing discovery and quietly selecting
CELL fallback. The largest supported canonical collection must fit the
negotiated payload and the caller-supplied guest TX staging before
advertisement. The selected Akashic `desktop-apt1` composition derives a
917,648-byte TX minimum from its full CELL, collection, and `DATA_GRAPHICS`
envelope; the 8 KiB focused Forth fixtures are not a product capacity.
`MANDATORY_CAPABILITIES` remains the base `0x3f`; collection support belongs
only to the additive `RET_CAPS` negotiation.
