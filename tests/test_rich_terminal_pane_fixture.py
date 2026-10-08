"""A constructed Desk pane preview preserves captured content and pointer maps."""

from dataclasses import replace
from pathlib import Path

import pytest

from rich_terminal.retained_scene import ObjectBounds
from rich_terminal.retained_view import GlyphRunDraw, PaneDraw
from tools.render_pane_preview import (
    DEFAULT_LAYOUT, construct_pane_offer, load_layout,
)
from tools.render_terminal_snapshot import load_offer


SNAPSHOT = Path(__file__).parent / "fixtures/compositor/desktop-six-app-final.json.gz"


def _fixture(**kwargs):
    offer, _, _ = load_offer(SNAPSHOT, -1)
    layout = load_layout(DEFAULT_LAYOUT)
    constructed, record = construct_pane_offer(offer, layout, **kwargs)
    return offer, constructed, record


def test_authored_pane_fixture_preserves_all_nondivider_draws_and_original_regions():
    original, preview, record = _fixture()
    assert preview.cell is original.cell
    assert preview.scope is original.scope
    assert record["preview_kind"] == "constructed pane publication over recorded Desk content"
    assert len(record["panes"]) == 6
    assert [pane["title"] for pane in record["panes"]] == [
        "Akashic Pad", "File Explorer", "Daybook", "Grid", "Agent", "Sound Lab",
    ]
    assert all(not pane["title_has_reserved_row"] for pane in record["panes"])
    removed_ids = {item["object_id"] for item in record["suppressed_glyph_runs"]}
    assert removed_ids
    by_id = {region.region_id: region for region in preview.retained.regions}
    for region in original.retained.regions:
        retained = by_id[region.region_id]
        assert replace(retained, draws=region.draws) == region
        assert retained.draws == tuple(
            draw for draw in region.draws
            if not isinstance(draw, GlyphRunDraw) or draw.object_id not in removed_ids
        )
    assert max(region.z_order for region in preview.retained.regions
               if region.region_id in record["placeholder_content_region_ids"]
               or region.region_id == record["chrome_region_id"]) < min(
                   region.z_order for region in original.retained.regions)


def test_pane_fixture_round_trips_through_real_display_offer_transport():
    from shared_session import display_offer_from_wire, display_offer_to_wire

    _, preview, _ = _fixture()
    assert display_offer_from_wire(display_offer_to_wire(preview)) == preview


def test_focus_and_outer_bounds_updates_keep_recorded_tile_content_fixed():
    original, preview, record = _fixture(
        focused_slot=6, bounds_overrides={1: (0, 0, 92, 41)},
    )
    assert [pane["slot"] for pane in record["panes"] if pane["focused"]] == [6]
    assert record["panes"][0]["outer_bounds"] == [0, 0, 92, 41]
    assert record["panes"][0]["content_bounds"] == [0, 0, 92, 41]
    chrome = next(region for region in preview.retained.regions
                  if region.region_id == record["chrome_region_id"])
    assert all(isinstance(draw, PaneDraw) for draw in chrome.draws)
    assert chrome.draws[-1].title == "Sound Lab"
    assert chrome.draws[-1].focused
    assert preview.cell is original.cell
    with pytest.raises(ValueError, match="beyond its tile and authored dividers"):
        _fixture(bounds_overrides={1: (0, 0, 94, 42)})
    with pytest.raises(ValueError, match="contain its fixed recorded tile"):
        _fixture(bounds_overrides={1: (1, 0, 92, 42)})


def test_partial_glyph_overlap_is_refused_without_changing_guest_text():
    original, _, _ = _fixture()
    root = original.retained.regions[0]
    glyph = next(draw for draw in root.draws if isinstance(draw, GlyphRunDraw))
    crossing = replace(glyph, object_id=999999, z_order=-999,
                       bounds=ObjectBounds(91, 3, 2, 1), text="AB")
    modified_root = replace(root, draws=(crossing, *root.draws))
    modified = replace(original, retained=replace(
        original.retained, regions=(modified_root, *original.retained.regions[1:]),
    ))
    with pytest.raises(ValueError, match="partially crosses authored divider"):
        construct_pane_offer(modified, load_layout(DEFAULT_LAYOUT))
    assert modified.retained.regions[0].draws[0].text == "AB"


@pytest.mark.parametrize("appearance_name", ("reference", "flowing"))
def test_real_compositor_preserves_content_pixels_and_pointer_targets(appearance_name):
    pygame = pytest.importorskip("pygame")
    from display import VirtualTerminal
    from rich_terminal.appearance import get_appearance
    from rich_terminal.font_set import FontSet
    from rich_terminal.pygame_view import RegionOcclusion, resolve_pointer
    from session_viewer import apply_terminal_snapshot, compose_terminal_frame_result

    original, preview, record = _fixture()
    pygame.font.init()
    try:
        font = FontSet(pygame, None, 12, (), styles={})
        terminal = VirtualTerminal(cols=original.cell.cols, rows=original.cell.rows)
        apply_terminal_snapshot(terminal, original.cell)

        def compose(offer):
            return compose_terminal_frame_result(
                pygame, terminal, font, font.cell_width, font.cell_height,
                retained_plane=offer.retained, show_cursor=False,
                appearance=get_appearance(appearance_name), glyph_cache={},
            )

        before, after = compose(original), compose(preview)
        added_regions = {record["chrome_region_id"], *record["placeholder_content_region_ids"]}
        assert tuple(entry for entry in after.hit_entries
                     if not isinstance(entry, RegionOcclusion)
                     or entry.region_id not in added_regions) == before.hit_entries
        for entry in before.hit_entries:
            x, y = (entry.rect.left + entry.rect.right) // 2, (entry.rect.top + entry.rect.bottom) // 2
            kwargs = dict(cell_width=font.cell_width, cell_height=font.cell_height)
            assert resolve_pointer(before.hit_entries, x, y, **kwargs) == resolve_pointer(
                after.hit_entries, x, y, **kwargs)

        assert pygame.image.tobytes(before.surface, "RGB") != pygame.image.tobytes(after.surface, "RGB")
        # Mask only the declared gutters. Every remaining pixel must still be
        # produced exactly as in the real captured scene, including all text.
        for x, y, cols, rows in record["divider_bounds"]:
            rect = pygame.Rect(x * font.cell_width, y * font.cell_height,
                               cols * font.cell_width, rows * font.cell_height)
            before.surface.fill((0, 0, 0), rect)
            after.surface.fill((0, 0, 0), rect)
        assert pygame.image.tobytes(before.surface, "RGB") == pygame.image.tobytes(after.surface, "RGB")
    finally:
        pygame.quit()
