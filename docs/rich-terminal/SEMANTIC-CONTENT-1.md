# SEMANTIC-CONTENT-1 protocol slice

Status: protocol value, wire codec, immutable retained model, server ingress,
renderer-neutral immutable draw projection, and exact shared-viewer transport
implemented. The reference Pygame sink now rasterizes every collection kind and
publishes immutable TAB hit targets from the exact paint pass. The paired
Akashic `desktop-apt1` producer now advertises the capability, projects ordinary
UIDL/canonical-widget values, and has exercised all four kinds plus
acknowledgement-bound TAB activation through that sink. MegaPad's terminal
core, guest module, shared host, and reference viewer also implement the
positioned `PLACE`, `EXTEND`, and `SCROLL` input specified below. A physical
renderer must not
advertise `RET_CONTROL_COLLECTIONS` until its compositor and acknowledgement
path can render every visible kind.

## Decision

Text areas, logical text grids, tabsets, and tabs extend the existing retained
`CONTROL` namespace. They do not create four message families, an applet scene
API, or terminal-buffer reservations.

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

Feature bit 9, `RET_CONTROL_COLLECTIONS`, gates all four kinds and depends on
bit 8 `RET_CONTROLS`. The same `CONTROL_DEFINE`, `CONTROL_REPLACE`,
`CONTROL_DROP`, owner authority, independent control-ID high-water, object
quota, UTF-8 quota, transaction, and exact-revision publication rules apply.
Menus remain valid with bit 8 alone.

This is one extensible semantic record with one shared text-collection body:

| Control kind | Value | Shape |
|---|---:|---|
| `TEXT_AREA` | 5 | bounded root plus one `STX1` logical text collection |
| `TEXT_GRID` | 6 | bounded root plus one `STX1` logical text collection |
| `TABSET` | 7 | bounded root, no content body |
| `TAB` | 8 | renderer-laid-out `TABSET` child using the existing label/shortcut fields |

The design is renderer-neutral. It carries logical rows, columns, spans,
stable item keys, logical-order text with a paragraph direction, a generic
viewport origin, authoritative state, and selection/caret positions. It does
not carry a retained-cell capacity, font, padding, pixel rectangle, refresh
waveform, e-paper cadence, or physical hit box. Characters, widths, ordering,
mirroring, and joining follow the shared text rules in `APT-1-TEXT.md`.

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
payload bytes and a retained transaction maximum of at least 352 bytes. There
is no second item-count or content-byte policy maximum; the negotiated frame
maximum, retained transaction maximum, object quota, owner aggregate UTF-8
quota, and caller's terminal allocation are the bounds.
Every CONTROL record consumes one existing object-quota slot and every STX1
item consumes one more; this accounts for stable retained values a selected
renderer may materialize without introducing a new kind-specific capacity.

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

Exactly `item_count` variable records follow. Each begins with the 32-byte
header `<QIIIIHHI>`, followed immediately by its UTF-8 text:

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

## Hierarchy and mutation

`TEXT_AREA`, `TEXT_GRID`, and `TABSET` are bounded roots with parent and order
zero. They have no label or shortcut. `TAB` is a label-bearing child of one
same-owner, same-region `TABSET`; it has renderer-owned child geometry and a
unique sibling order. At most one visible/enabled tab is `SELECTED` per tabset.
TEXT_AREA, TEXT_GRID, and TAB admit `VISIBLE`, `ENABLED`, and `SELECTED`;
TABSET admits `VISIBLE` and `ENABLED`. As elsewhere, `SELECTED` requires the
same control to be visible and enabled.

Menu and tabset replacements retain the existing state-only rule. `TAB`
replacement may change state, label, or shortcut while preserving identity and
hierarchy. Text area/grid replacement may change state and the complete
semantic content while preserving identity and geometry; a changed body must
carry a strictly newer content revision. UTF-8 usage is removed and added
atomically against the owner's existing aggregate reservation.

`CONTROL_EVENT` activation is sufficient for `TAB` and remains revision-bound.
Existing revision-bound KEY/TEXT input remains usable by the authoritative
focused UI. Pointer input on text areas and grids uses the positioned kinds
below.

## Positioned input

`CONTROL_EVENT` kinds 2 to 4 let a pointer act on text areas and grids without
the terminal guessing application state. They require feature bit 9. Their
position tail names the exact acknowledged content: `content_revision` is the
control's STX1 content revision in the composite named by `model_revision`,
`item_key` names an item carried in that content, and `scalar_offset` is a
Unicode-scalar boundary within that item. The terminal computes a position
from its own presentation of the content and emits an event only for a
visible, effectively enabled root. `READ_ONLY` content admits all three kinds,
because they move the caret, selection, or viewport, never the text.

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

The client revalidates the owner, generation, control identity, kind, event
revision, and content revision, and resolves the item key in the content it
published. It then routes the position to the ordinary widget that produced
that content, which applies it to its current state and clamps it if that
state has since moved on. A stale or unknown position is discarded. The
terminal never changes the caret, selection, or viewport itself; the client
publishes the result in a later transaction.

## Cost boundary

STX1 adds one 72-byte collection header and one 32-byte header per carried
text item. Decode and canonical validation use linear scalar/key passes. The
common one-row-span case uses a linear overlap pass; genuine row spans use an
`O(n log n)` rectangle sweep. They do not compute content hashes, rasterize,
scan terminal cells, or rebuild a second scene. Immutable values cache their
validated UTF-8 and wire byte totals, so quota admission and scene freezing do
not re-encode every string. That same canonical item loop derives exactly two
non-semantic summaries: whether the content has TEXT_AREA shape and how many
items carry `CURRENT`. Later scene, view, and shared-wire family checks consult
those immutable facts in `O(1)` instead of rescanning items. They add no hash,
certificate, cache, traversal, or wire field; canonical STX1 construction and
decode remain the boundary that proves the facts. Wire encoding still makes
one necessary UTF-8/body pass. Each item uses one existing object-quota slot,
so one accepted control replaces many per-row GLYPH_RUN definitions without
evading the caller's retained-value bound. `CONTROL_REPLACE` currently resends
the complete small collection.

That full replacement is the bounded first slice, not a claim that it is the
best steady-state Pad keystroke transport. Before adding machinery, measure its
guest instructions and exact UART bytes against the residual-glyph path. If
the complete visible text area becomes the bottleneck, the next protocol work
is one generic revision-bound STX1 item patch operation with atomic model
application—not Pad-specific events, a grid-only message family, hashes, or a
renderer cache exposed on the wire.

## Shared-viewer transport

The local JSON display-offer wire carries the renderer-facing values with exact
tags `text_area`, `text_grid`, `tabset`, and nested `tab`. Text collection roots
use the exact fields `kind`, `control_id`, `state`, `order`, `z_order`,
`bounds`, and `content_stx1_base64`. A tabset replaces the content field with
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
scalar-column slots. Missing rows remain blank, U+0009 is a renderer-owned blank
logical slot in this first policy, the anchor-to-primary range receives a
half-open selection fill, and the primary scalar boundary receives a persistent
caret. An offscreen endpoint remains authoritative but is never moved to the
viewport origin. The reference sink uses the terminal monospace font for this
editor policy.

TEXT_GRID maps item rectangles directly from their logical viewport-relative
row, column, and span values. It paints role, primary, `CURRENT`, and
`UNAVAILABLE` states with renderer-owned styling and never materializes a
rows-by-columns matrix. TABSET uses renderer-owned sans-serif metrics: natural
tab widths when they fit and deterministic equal partitioning when they do not.
Only physically visible, effectively enabled TAB children enter the immutable
hit map as activation targets. An enabled TEXT_AREA or TEXT_GRID root enters it
as a text target that keeps the exact partition its paint pass used, so a
point maps to the item and scalar slot drawn there (a point on a slot names
the boundary before it). A disabled text root, and every menu bar, tabset, and
open popup, enters as a control surface: it blocks lower controls and never
starts a raw pointer gesture. A point covered only by a region barrier, or by
nothing, shows CELL or residual content and may start one.

The reference viewer routes a left press on a text target as `PLACE` (as
`EXTEND` with Shift on a text area), a drag that began there as `EXTEND` at the
clamped position, and wheel input there as `SCROLL`. Presses, drags, releases,
and wheel steps on residual content become raw `POINTER` input at the cell
under the pointer, with a release that cannot yet be sent kept until the
acknowledged display is current.

Raster code consumes the already validated immutable draw/content values. It
does not encode or decode STX1, rerun family/UTF-8/overlap proofs, or render an
unbounded whole collection string into one temporary surface. Grid and tab text
uses one-scalar glyph surfaces and emits only glyph pixels that intersect the
physical clip. That bounds each raster allocation and render call; it does not
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
isolated policy bit flip. The MegaPad guest module accepts mask `0x33f`, requires bit 8 for
bit 9, and evolves the one public CONTROL writer to copy caller-bounded kinds 5
through 8 without a parallel message or legacy encoder. It enforces exact root,
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
