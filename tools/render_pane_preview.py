#!/usr/bin/env python3
"""Preview explicitly authored pane chrome over the recorded six-app Desk.

This is a constructed publication fixture, not a capture of migrated Akashic
pane producers. The layout manifest supplies every pane and divider coordinate;
no borders, titles, ownership, or focus are inferred from displayed text.

The original content regions and controls stay in their recorded positions.
Empty content-region bindings model future guest publication below the original
regions. Only glyph runs wholly inside authored divider cells are suppressed.
"""

from __future__ import annotations

import argparse
from dataclasses import replace
import gzip
import hashlib
import json
from pathlib import Path
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

from tools import render_terminal_snapshot as snapshot_renderer

DEFAULT_LAYOUT = ROOT / "tests/fixtures/compositor/desktop-six-pane-layout.json"


def _rectangle(value, label):
    if (not isinstance(value, (list, tuple)) or len(value) != 4
            or any(type(part) is not int for part in value)
            or value[2] <= 0 or value[3] <= 0):
        raise ValueError(f"{label} must be four integers x, y, positive cols, positive rows")
    return tuple(value)


def _intersection(left, right):
    x = max(left[0], right[0])
    y = max(left[1], right[1])
    end_x = min(left[0] + left[2], right[0] + right[2])
    end_y = min(left[1] + left[3], right[1] + right[3])
    return (x, y, end_x - x, end_y - y) if end_x > x and end_y > y else None


def _subtract(rectangle, cut):
    overlap = _intersection(rectangle, cut)
    if overlap is None:
        return (rectangle,)
    x, y, width, height = rectangle
    ox, oy, ow, oh = overlap
    pieces = (
        (x, y, width, oy - y),
        (x, oy + oh, width, y + height - oy - oh),
        (x, oy, ox - x, oh),
        (ox + ow, oy, x + width - ox - ow, oh),
    )
    return tuple(piece for piece in pieces if piece[2] and piece[3])


def _outside(rectangle, covers):
    remaining = (rectangle,)
    for cover in covers:
        remaining = tuple(piece for part in remaining for piece in _subtract(part, cover))
    return remaining


def load_layout(path):
    layout = json.loads(Path(path).read_text(encoding="utf-8"))
    if not isinstance(layout, dict) or layout.get("format") != "megapad-desk-pane-preview-1":
        raise ValueError("layout must be a megapad-desk-pane-preview-1 manifest")
    return layout


def construct_pane_offer(offer, layout, *, focused_slot=1, bounds_overrides=None):
    """Return the constructed offer plus its explicit transformation record.

    Bounds overrides change only a pane's outer chrome. Its recorded content
    rectangle remains fixed, and any newly covered cell must be in a declared
    divider. The helper refuses a partially intersecting glyph run instead of
    silently dropping or rewriting guest text.
    """
    from rich_terminal.retained_scene import ObjectBounds
    from rich_terminal.retained_view import GlyphRunDraw, PaneDraw, RetainedRegionDraw

    if [offer.cell.cols, offer.cell.rows] != layout["cells"]:
        raise ValueError("snapshot geometry does not match the authored layout")
    panes = layout["panes"]
    slots = {pane["slot"] for pane in panes}
    if len(slots) != len(panes) or focused_slot not in slots | {0}:
        raise ValueError("pane slots must be unique and focus must name a slot or zero")
    overrides = dict(bounds_overrides or {})
    if not overrides.keys() <= slots:
        raise ValueError("bounds override names an unknown pane slot")
    dividers = tuple(_rectangle(rect, "divider") for rect in layout["divider_bounds"])
    screen = (0, 0, offer.cell.cols, offer.cell.rows)
    if any(_outside(rect, (screen,)) for rect in dividers):
        raise ValueError("authored divider lies outside the snapshot")

    owner = layout["owner_id"], layout["owner_generation"]
    chrome_id = layout["chrome_region_id"]
    original_regions = offer.retained.regions
    if not original_regions:
        raise ValueError("pane preview requires recorded retained content")
    region_ids = {region.region_id for region in original_regions
                  if (region.owner_id, region.owner_generation) == owner}
    object_ids = {draw.object_id for region in original_regions
                  if (region.owner_id, region.owner_generation) == owner
                  for draw in region.draws if hasattr(draw, "object_id")}
    added_region_ids = [chrome_id, *(pane["content_region_id"] for pane in panes)]
    added_object_ids = [pane["object_id"] for pane in panes]
    if (len(set(added_region_ids)) != len(added_region_ids)
            or region_ids.intersection(added_region_ids)
            or len(set(added_object_ids)) != len(added_object_ids)
            or object_ids.intersection(added_object_ids)):
        raise ValueError("authored preview identities collide with the recorded scene")

    chrome_z = min(region.z_order for region in original_regions) - 2
    chrome_draws = []
    content_regions = []
    pane_records = []
    for pane in panes:
        tile = _rectangle(pane["tile_bounds"], "tile bounds")
        outer = _rectangle(overrides.get(pane["slot"], pane["outer_bounds"]), "outer bounds")
        if _outside(tile, (screen,)) or _outside(tile, (outer,)):
            raise ValueError("pane outer bounds must contain its fixed recorded tile")
        if _outside(outer, (tile, *dividers)):
            raise ValueError("pane chrome extends beyond its tile and authored dividers")
        content = (tile[0] - outer[0], tile[1] - outer[1], tile[2], tile[3])
        focused = pane["slot"] == focused_slot
        chrome_draws.append(PaneDraw(
            object_id=pane["object_id"], z_order=int(focused),
            bounds=ObjectBounds(*outer), content_region_id=pane["content_region_id"],
            content_bounds=ObjectBounds(*content), title=pane["title"], focused=focused,
        ))
        content_regions.append(RetainedRegionDraw(
            owner_id=owner[0], owner_generation=owner[1],
            region_id=pane["content_region_id"],
            logical_x=tile[0], logical_y=tile[1], logical_cols=tile[2], logical_rows=tile[3],
            clip_x=tile[0], clip_y=tile[1], clip_cols=tile[2], clip_rows=tile[3],
            z_order=chrome_z + 1, clipped=True, draws=(),
        ))
        pane_records.append({
            **pane, "outer_bounds": list(outer), "content_bounds": list(content),
            "focused": focused,
            "title_has_reserved_row": content[1] >= 1 and outer[2] >= 3,
        })

    changed_regions = []
    suppressed = []
    for region in original_regions:
        retained_draws = []
        for draw in region.draws:
            if isinstance(draw, GlyphRunDraw):
                physical = (
                    region.logical_x + draw.bounds.cell_x,
                    region.logical_y + draw.bounds.cell_y,
                    draw.bounds.cell_cols, draw.bounds.cell_rows,
                )
                if any(_intersection(physical, divider) for divider in dividers):
                    if draw.parent_bounds or _outside(physical, dividers):
                        raise ValueError(
                            f"glyph run {draw.object_id} partially crosses authored divider "
                            "coverage; author a fixture with separate chrome runs"
                        )
                    suppressed.append({
                        "owner_id": region.owner_id, "region_id": region.region_id,
                        "object_id": draw.object_id, "bounds": list(physical),
                    })
                    continue
            retained_draws.append(draw)
        changed_regions.append(replace(region, draws=tuple(retained_draws)))

    chrome = RetainedRegionDraw(
        owner_id=owner[0], owner_generation=owner[1], region_id=chrome_id,
        logical_x=0, logical_y=0, logical_cols=offer.cell.cols, logical_rows=offer.cell.rows,
        clip_x=0, clip_y=0, clip_cols=0, clip_rows=0,
        z_order=chrome_z, clipped=False,
        draws=tuple(sorted(chrome_draws, key=lambda draw: (draw.z_order, draw.object_id))),
    )
    regions = tuple(sorted(
        [chrome, *content_regions, *changed_regions],
        key=lambda region: (region.z_order, region.owner_id, region.region_id),
    ))
    constructed = replace(offer, retained=replace(offer.retained, regions=regions))
    return constructed, {
        "preview_kind": layout["preview_kind"], "source": layout["source"],
        "notes": layout["notes"], "panes": pane_records,
        "chrome_region_id": chrome_id,
        "placeholder_content_region_ids": [region.region_id for region in content_regions],
        "divider_bounds": [list(rect) for rect in dividers],
        "suppressed_glyph_runs": suppressed,
    }


def build_parser():
    parser = snapshot_renderer.build_parser()
    parser.description = __doc__
    parser.add_argument("--layout", type=Path, default=DEFAULT_LAYOUT)
    parser.add_argument("--focus", type=int, default=1, metavar="SLOT",
                        help="focused authored pane slot; zero clears focus (default: 1)")
    parser.add_argument("--pane-bounds", nargs=5, type=int, action="append", default=[],
                        metavar=("SLOT", "X", "Y", "COLS", "ROWS"),
                        help="override outer chrome around a fixed tile; repeat per pane")
    parser.add_argument("--offer", type=Path,
                        help="also save the constructed display offer as JSON or JSON.gz")
    return parser


def render(args):
    from shared_session import display_offer_to_wire

    layout = load_layout(args.layout)
    digest = hashlib.sha256(args.snapshot.read_bytes()).hexdigest()
    if digest != layout["snapshot_sha256"]:
        raise ValueError("snapshot hash does not match the explicitly authored layout")
    bounds = {}
    for slot, x, y, cols, rows in args.pane_bounds:
        if slot in bounds:
            raise ValueError(f"pane slot {slot} has multiple bounds overrides")
        bounds[slot] = (x, y, cols, rows)
    offer, selected, count = snapshot_renderer.load_offer(args.snapshot, args.frame)
    constructed, provenance = construct_pane_offer(
        offer, layout, focused_slot=args.focus, bounds_overrides=bounds,
    )
    encoded = json.dumps(display_offer_to_wire(constructed), ensure_ascii=False).encode("utf-8")
    metadata_path = args.output.with_suffix(".pane-preview.json")
    inputs = {args.snapshot.resolve(), args.layout.resolve()}
    outputs = [args.output, metadata_path, *([args.offer] if args.offer else [])]
    resolved_outputs = [path.resolve() for path in outputs]
    if len(set(resolved_outputs)) != len(outputs) or inputs.intersection(resolved_outputs):
        raise ValueError("preview outputs must be distinct and cannot replace inputs")
    if args.offer and not (args.offer.name.endswith(".json") or args.offer.name.endswith(".json.gz")):
        raise ValueError("constructed offer must have a .json or .json.gz extension")
    # Round-trip the authored offer through the same decoder used for real
    # captures, then call the existing production-compositor rendering tool.
    with tempfile.TemporaryDirectory(prefix="megapad-pane-preview-") as temporary:
        staged = Path(temporary) / "constructed-offer.json"
        staged.write_bytes(encoded)
        render_args = argparse.Namespace(**vars(args))
        render_args.snapshot = staged
        render_args.frame = 0
        result = snapshot_renderer.render(render_args)
    result.update(provenance)
    result.update({
        "snapshot": str(args.snapshot.resolve()), "snapshot_sha256": digest,
        "layout": str(args.layout.resolve()), "frame": selected, "frame_count": count,
        "metadata": str(metadata_path.resolve()),
    })
    if args.offer:
        args.offer.parent.mkdir(parents=True, exist_ok=True)
        args.offer.write_bytes(gzip.compress(encoded, mtime=0)
                               if args.offer.name.endswith(".gz") else encoded)
        result["constructed_offer"] = str(args.offer.resolve())
    metadata_path.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    return result


def main():
    parser = build_parser()
    args = parser.parse_args()
    try:
        result = render(args)
    except (OSError, ValueError, TypeError, ImportError, KeyError) as exc:
        parser.exit(2, f"{parser.prog}: {exc}\n")
    # Full transformation detail remains in the provenance sidecar.
    print(json.dumps({key: value for key, value in result.items()
                      if key != "suppressed_glyph_runs"}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
