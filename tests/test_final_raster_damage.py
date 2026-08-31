"""Focused final-raster damage and physical acknowledgement contracts."""

from __future__ import annotations

from dataclasses import replace

import pytest

from rich_terminal.cell_model import Cell, Cursor, TerminalView
from rich_terminal.display_cadence import DisplayCadenceScheduler
from rich_terminal.final_raster import (
    FinalRaster,
    FinalRasterDisplayState,
    FinalRasterToken,
    RasterDamageGrid,
    RasterDamageRect,
    derive_raster_damage,
)
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import RetainedFeature, RetainedPolicy
from rich_terminal.retained_view import DisplayScope
from rich_terminal.update_authority import TerminalGeometry


def _raster(
    width: int,
    height: int,
    pixels,
    *,
    bytes_per_pixel: int = 1,
    pixel_format: str = "TEST8",
) -> FinalRaster:
    return FinalRaster(
        width,
        height,
        bytes_per_pixel,
        pixel_format,
        bytes(pixels),
    )


def _scope(revision: int) -> DisplayScope:
    return DisplayScope(1, 2, 0, revision, 0, revision, revision)


def _policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE,
        max_owner_records=1,
        max_live_owners=1,
        max_regions=1,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=1,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=248,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=64,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=248,
    )


def _view(revision: int) -> CompositeTerminalView:
    geometry = TerminalGeometry(1, 1)
    return CompositeTerminalView(
        presentation_epoch=0,
        revision=revision,
        geometry=geometry,
        cell=TerminalView(
            attachment_epoch=1,
            session_id=2,
            presentation_epoch=0,
            revision=revision,
            cols=1,
            rows=1,
            cells=((Cell(ord(" "), 7, 0),),),
            dirty_spans=(),
            cursor=Cursor(0, 0, True),
        ),
        retained=None,
    )


def test_damage_comes_from_exact_final_pixels_and_coalesces_rectangles() -> None:
    baseline = _raster(5, 4, bytes(20))
    changed = bytearray(20)
    changed[4] = 1
    changed[6:8] = b"\x01\x01"
    changed[11:13] = b"\x01\x01"
    candidate = _raster(5, 4, changed)

    assert derive_raster_damage(baseline, candidate) == (
        RasterDamageRect(4, 0, 5, 1),
        RasterDamageRect(1, 1, 3, 3),
    )
    assert derive_raster_damage(candidate, candidate) == ()


def test_sink_selected_grid_expands_damage_without_semantic_geometry() -> None:
    baseline = _raster(5, 3, bytes(15))
    changed = bytearray(15)
    changed[1] = 1
    changed[14] = 1

    assert derive_raster_damage(
        baseline,
        _raster(5, 3, changed),
        grid=RasterDamageGrid(2, 2),
    ) == (
        RasterDamageRect(0, 0, 2, 2),
        RasterDamageRect(4, 2, 5, 3),
    )


def test_no_baseline_or_changed_raster_shape_requires_one_full_refresh() -> None:
    first = _raster(2, 2, b"\x00\x00\x00\x00")
    wider = _raster(3, 2, bytes(6))
    other_format = _raster(
        2,
        2,
        bytes(8),
        bytes_per_pixel=2,
        pixel_format="TEST16",
    )

    assert derive_raster_damage(None, first) == (
        RasterDamageRect(0, 0, 2, 2),
    )
    assert derive_raster_damage(first, wider) == (
        RasterDamageRect(0, 0, 3, 2),
    )
    assert derive_raster_damage(first, other_format) == (
        RasterDamageRect(0, 0, 2, 2),
    )


def test_busy_offer_pins_raster_damage_and_hit_map_until_exact_ack() -> None:
    state = FinalRasterDisplayState()
    first_raster = _raster(2, 1, b"\x00\x00")
    first_hit_map = (object(),)
    first = state.offer(
        FinalRasterToken(1, _scope(1)),
        first_raster,
        first_hit_map,
    )

    assert state.acknowledged is None
    assert state.offered is first
    assert first.raster is first_raster
    assert first.damage == (RasterDamageRect(0, 0, 2, 1),)
    assert first.hit_map == first_hit_map

    # Refresh-command acceptance, controller BUSY, and READY-before-settle do
    # not call acknowledge and therefore cannot move the physical baseline.
    with pytest.raises(RuntimeError, match="already awaiting physical ACK"):
        state.offer(
            FinalRasterToken(2, _scope(2)),
            _raster(2, 1, b"\x01\x00"),
        )
    with pytest.raises(RuntimeError, match="exact outstanding"):
        state.acknowledge(replace(first))
    assert state.acknowledged is None
    assert state.offered is first

    state.acknowledge(first)
    assert state.acknowledged is first
    assert state.offered is None

    second = state.offer(
        FinalRasterToken(2, _scope(2)),
        _raster(2, 1, b"\x01\x00"),
        (object(),),
    )
    assert second.damage == (RasterDamageRect(0, 0, 1, 1),)
    state.revoke(second)
    assert state.acknowledged is first
    assert state.offered is None


def test_latest_logical_view_stays_in_existing_cadence_while_sink_is_busy() -> None:
    cadence = DisplayCadenceScheduler(policy=_policy(), monotonic_us=lambda: 0)
    cadence.replace_session(1, 2)
    first_view = _view(1)
    cadence.submit(first_view)
    assert cadence.service() is first_view

    sink = FinalRasterDisplayState()
    physical_offer = sink.offer(
        FinalRasterToken(1, _scope(1)),
        _raster(1, 1, b"\x01"),
        ("first-hit-map",),
    )
    cadence.submit(_view(2))
    latest = _view(3)
    cadence.submit(latest)

    assert cadence.service() is None
    assert cadence.offered_revision == 1
    assert cadence.pending_revision == 3
    assert sink.offered is physical_offer
    assert sink.acknowledged is None

    sink.acknowledge(physical_offer)
    cadence.acknowledge(first_view)
    assert cadence.service() is latest


def test_final_raster_storage_is_exactly_geometry_bounded_and_immutable() -> None:
    source = bytearray((1, 2, 3, 4, 5, 6))
    raster = FinalRaster(2, 1, 3, "RGB888", source)
    source[0] = 99

    assert raster.pixels == b"\x01\x02\x03\x04\x05\x06"
    with pytest.raises(ValueError, match="byte count"):
        FinalRaster(2, 1, 3, "RGB888", b"\x00")
    with pytest.raises(TypeError, match="bytes-like"):
        FinalRaster(1, 1, 3, "RGB888", 3)
