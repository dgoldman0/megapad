# Repainting only what a frame changes

Status: design, not built. 2026-09-29.

The shared-session viewer composes every display offer from scratch: it
renders all 23,520 CELL cells of the 280x84 Desktop, then paints every
retained draw over them. A typed character changes one text area and one
status glyph run. This design repaints only the pixels an offer or the
viewer's own state can change, and keeps every frame exactly what a full
composition would give.

## Why

Composing one Desktop typing offer takes 42 to 47 ms on a quiet host
(Akashic `local_testing/evidence/simulator-host-lag-20260929.md`). Measured
piece by piece on a real offer under host load, a full composition took
63 ms: the CELL pass 34 ms, the 695 glyph runs 12 ms, and one item view
12 ms. That item view, and five other controls, are republished every frame
with only a new content revision. The editor's text area that the key
changed is 54x36 cells and paints in 0.3 ms.

## What must hold

A partially repainted frame has exactly the pixels, and exactly the hit map,
that a full composition of the same offer and viewer state has. Partial
repaint is only a cheaper way to reach that frame. Where exactness cannot be
shown cheaply, the viewer composes in full.

## Composition as ordered paint operations

A full composition is a fixed sequence of paint operations:

1. fill the frame with the CELL default background;
2. each CELL row in order, its cell backgrounds and then its glyphs,
   decorations and cursor-free marks;
3. each retained region in plane order: its draws in order, then the popups
   its menu bars opened;
4. the CELL cursor.

Each operation changes only pixels inside its extent. Inside its extent, its
result depends only on its own inputs and the pixels already there. So the
final value of a pixel is the result of the operations whose extents hold
it, applied in order.

If none of those operations changed its inputs, its extent or its place in
the order since the previous frame, the pixel keeps its previous value. The
damage is therefore the union of the previous and the new extents of every
operation that changed. Repainting a damage rectangle D means: fill D with
the default background, then run, under the clip D and in order, every
operation whose extent meets D. Inside D this reproduces the full
composition, and outside D nothing changes.

## Extents

- A CELL cell: its box of `span` cells, extended right and down by its
  glyph's overhang. The CELL renderer records the largest overhang it has
  drawn, in cells, for each direction. The reference Desktop font has none:
  its glyphs are fitted to their cells, and its line height equals its
  glyph height.
- An object draw (GLYPH_RUN, POLYLINE, IMAGE, READOUT, METER, STATUS, PLOT,
  WAVEFORM): its object rectangle within the region viewport, which is where
  every object painter clips.
- A TEXT_AREA, TEXT_GRID, ITEM_VIEW or TABSET: its anchor within the region
  viewport.
- A MENU_BAR: its anchor and its shadow within the region viewport, and its
  popups, which may paint anywhere in the region viewport.
- The cursor: its cell.

## Painters under a smaller clip

Every painter derives its clip from the surface's current clip, through
`_region_viewport` or by intersecting with it, so an outer clip keeps all of
its pixels inside D. The painters fall into two classes.

**Exact under any clip.** Inside the clip they paint exactly what a full
paint would: CELL cells; GLYPH_RUN, whose glyphs are cropped to their own
slots and whose fill and decorations are axis-aligned; READOUT and METER,
which are clipped fills and text; STATUS, whose shape is tested per pixel in
frame coordinates; IMAGE, sampled per pixel in frame coordinates; and the
pixels of TEXT_AREA, TEXT_GRID and ITEM_VIEW, which are an opaque fill,
per-slot glyphs, clipped fills and clipped borders.

**Whole only.** POLYLINE, PLOT and WAVEFORM clip diagonal lines and fill
polygons to the current clip with integer rounding (`_clip_line_segment`,
`_alpha_polygon`), so a smaller clip can move a pixel. MENU_BAR, with its
popups, and TABSET draw their lines through the same clipping, which also
caps a line's width by the visible size, and they change with hover and
press. When a whole-only draw meets D, D grows to hold its whole extent, and
this repeats until D is stable. Such a draw then paints under exactly the
clip a full composition gives it.

## Hit map

The hit map is assembled region by region and draw by draw, in painter
order:

- A region's occlusion entry is kept while its header and the surface are
  unchanged, and otherwise comes from the region's repaint.
- A draw equal to its previous value keeps its previous entries.
- A TEXT_AREA, TEXT_GRID or ITEM_VIEW whose content differs from its
  previous content only in `content_revision` keeps its previous entries
  with that revision replaced, and causes no damage. No painter reads the
  content revision. Only the entries carry it, because positioned input is
  bound to it.
- Any other changed control takes its entries from this frame's repaint,
  which always paints it whole. Its entries depend on its paint clip: a text
  root's rectangle and an item view's item areas are clipped to it.
- Entries that a repaint produces for an unchanged control are discarded.

A frame keeps, for the next one, each region's and each draw's entries by
region and draw identity, and the values they came from.

## Damage from an offer and the viewer's state

- CELL: each row whose snapshot row is a different object and not equal to
  the previous row, as a full-width band extended down by the recorded
  overhang.
- Cursor: the previous and the new cursor cell, when its position,
  visibility or blink phase changes.
- Regions added, removed or reordered, or with a changed header: the
  previous and the new region viewport.
- Draws added, removed or changed in a kept region, except for a
  revision-only change: their previous and new extents.
- A changed series history: the whole extents of the plots and waveforms
  that read it.
- A changed hover or press: the whole viewports of the regions holding the
  previous and the new control.

The viewer composes in full instead for the first frame; after a window,
font or cell size change or an SDL window repaint event; when retained
visibility or initialization changes; when an IMAGE manifest changes; and
when the damage covers more than half the frame.

## The CELL pass

`VirtualTerminal.render` is split into the full render and a partial paint
of a range of rows and columns into an existing surface, both using the same
per-cell code. A damage rectangle repaints the rows it meets, and the rows
above whose recorded downward overhang reaches it, and the columns it meets,
and the columns to its left within the recorded rightward overhang. Each row
still paints its backgrounds before its glyphs. Coverage by opaque glyph-run
fills may still skip a cell: a covered cell in D is painted over by its
covering run, which meets D and is therefore repainted.

## Presenting

The frame surface is updated in place. Only damaged rectangles are copied to
the window, and the SDL reference sink's completion boundary becomes
`pygame.display.update` over those rectangles. A full composition still
flips the whole window. A damage-aware sink still captures the complete
frame surface.

## Proof

- Replay tests compose sequences of real offers both ways, incrementally and
  in full, and require identical RGBA pixels and identical hit maps at every
  frame. The sequences cover typing, menus opening and closing, hover and
  press, Daybook edits, and Sound Lab series.
- Synthetic sequences apply random edits to the existing synthetic planes:
  partial coverage, translucent fills, offset regions, region clips, every
  glyph attribute, and an overhanging font.
- Each of these breaks must fail those tests: an extent one pixel too small;
  a whole-only draw treated as exact; no overhang margin; a repaint's entries
  kept for an unchanged control; a revision-only change without the new
  revision in its entries.

## Expected effect

For a typed key the damage is the editor's text area and the status glyph
run. Measured under load, repainting that area is about 0.3 ms of text area
paint and about 1,944 CELL cells, roughly 3 ms, against a 63 ms full
composition. The six controls republished with only a new revision are not
repainted.

## Order of work

1. Split the CELL renderer into full and partial paints, with no change to
   any frame.
2. Record each frame's per-region and per-draw entries and extents from the
   full composition, with no change to any frame.
3. Compute damage, repaint it, and assemble the hit map, held to the full
   composition by the replay and synthetic tests.
4. Copy and present only damaged rectangles.

The guest still sends, and the device still admits, the controls it
republishes with only a new revision. That repeat belongs to the guest and
is tracked separately.
