# SEMANTIC-CONTENT-1 protocol slice

Status: protocol value, wire codec, immutable retained model, server ingress,
and renderer-neutral immutable draw projection implemented. Pygame
rasterization/hit maps, Akashic production, and text/grid item input are
deliberately not implemented in this slice. A physical renderer must not
advertise `RET_CONTROL_COLLECTIONS` until its compositor and acknowledgement
path can render every visible kind.

## Decision

Text areas, logical text grids, tabsets, and tabs extend the existing retained
`CONTROL` namespace. They do not create four message families, an applet scene
API, or terminal-buffer reservations.

The wire does not equate one CONTROL root with one UIDL source element. One
generic provider element may publish multiple roots—such as a TABSET and a
TEXT_AREA—using stable control IDs derived by the producer from its attachment,
source index, and stable per-element object key. Those producer coordinates do
not enter the wire. The results remain ordinary independent CONTROL
definitions in one owner and region, not a mirrored DOM or element-owned scene.

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
stable item keys, text, a generic viewport origin, authoritative state, and
selection/caret positions. It does not carry a retained-cell capacity, font,
padding, pixel rectangle, refresh waveform, e-paper cadence, or physical hit
box.

## CONTROL envelope

The existing 80-byte prefix remains `<QQQHHiQQIIIIIIII>`. Its last three u32
fields are now `label_bytes`, `shortcut_bytes`, and `content_bytes`. The exact
payload is:

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
unknown advertised bits, so a terminal policy enabling bit 9 requires the
synchronized Akashic mask/encoder update; a bit-8-only policy remains the exact
menu-compatible path during that transition.

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

Content flag bit 0 is `READ_ONLY`; all other bits are zero.

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
cannot be current. Text is well-formed Unicode scalar UTF-8 and contains no C0
control scalar other than U+0009 HORIZONTAL TAB, and no DEL. A tab remains one
scalar for primary/anchor offsets; its visual expansion belongs to the
renderer, like font metrics and wrapping. Offsets count Unicode scalar values,
not UTF-8 bytes or grapheme clusters.

Rows and columns on every item are absolute document/grid coordinates. The
origin and positive extents define the exact half-open logical viewport
rectangle. The selected renderer maps only that rectangle into the root bounds
and must not expose additional logical rows or columns merely because its font
leaves spare pixels. For TEXT_AREA the column coordinates count Unicode
scalars before visual tab expansion; for TEXT_GRID they are logical grid
columns.

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
carried line has at most `columns` Unicode scalars, so the
horizontal origin and selection offsets share one exact logical coordinate.
Primary and anchor name the caret and optional selection endpoint. `TEXT_GRID`
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
focused UI. Renderer-owned text-area pointer placement and text-grid item
activation require an item-key/content-revision input extension; this slice
intentionally does not guess that event contract. Until it exists, the two
kinds do not claim item-addressed native pointer input.

## Cost boundary

STX1 adds one 72-byte collection header and one 32-byte header per carried
text item. Decode and canonical validation use linear scalar/key passes. The
common one-row-span case uses a linear overlap pass; genuine row spans use an
`O(n log n)` rectangle sweep. They do not compute content hashes, rasterize,
scan terminal cells, or rebuild a second scene. Immutable values cache their
validated UTF-8 and wire byte totals, so quota admission and scene freezing do
not re-encode every string. Wire encoding still makes one necessary UTF-8/body
pass. Each item uses one existing object-quota slot, so one accepted control
replaces many per-row GLYPH_RUN definitions without evading the caller's
retained-value bound. `CONTROL_REPLACE` currently resends the complete small
collection.

That full replacement is the bounded first slice, not a claim that it is the
best steady-state Pad keystroke transport. Before adding machinery, measure its
guest instructions and exact UART bytes against the residual-glyph path. If
the complete visible text area becomes the bottleneck, the next protocol work
is one generic revision-bound STX1 item patch operation with atomic model
application—not Pad-specific events, a grid-only message family, hashes, or a
renderer cache exposed on the wire.

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
  repeat UTF-8 and rectangle-overlap validation on every display offer.

The next MegaPad slice must carry these draw values through `shared_session.py`,
then add one ordinary pygame compositor path and immutable hit map. Until both
exist, the reference sink rejects a visible collection draw rather than
silently reducing it to CELL or glyphs.
Akashic can advertise/use bit 9 only after its generic UIDL semantic provider
and CONTROL encoder emit these exact records. Pad and Daybook must remain
ordinary UIDL/TUI sources; neither receives a terminal API or renderer-specific
annotation.
