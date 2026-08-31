"""Sink-local damage and acknowledgement state for final terminal rasters.

The APT cadence scheduler owns logical latest-view coalescing.  This module
starts after a selected renderer has composed CELL, every rich plane, and the
cursor into one packed raster.  It deliberately retains no second pending
revision queue: a physical sink owns only its last acknowledged pixel baseline
and one exact outstanding presentation while its controller is busy or
settling.
"""

from __future__ import annotations

import operator
from dataclasses import dataclass

from .retained_view import DisplayScope


def _positive_integer(name: str, value) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        normalized = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if normalized < 1:
        raise ValueError(f"{name} must be positive")
    return int(normalized)


@dataclass(frozen=True, slots=True)
class FinalRaster:
    """One immutable, tightly packed renderer output image.

    ``pixel_format`` is an opaque sink-local format identity.  Damage compares
    pixels only when geometry, format identity, and bytes-per-pixel agree; a
    change in any of them damages the complete candidate raster.
    """

    width: int
    height: int
    bytes_per_pixel: int
    pixel_format: str
    pixels: bytes

    def __post_init__(self) -> None:
        for name in ("width", "height", "bytes_per_pixel"):
            object.__setattr__(
                self,
                name,
                _positive_integer(name, getattr(self, name)),
            )
        if not isinstance(self.pixel_format, str):
            raise TypeError("pixel_format must be str")
        if not self.pixel_format:
            raise ValueError("pixel_format must be nonempty")
        try:
            pixel_view = memoryview(self.pixels)
        except TypeError as exc:
            raise TypeError("pixels must be bytes-like") from exc
        immutable_pixels = bytes(pixel_view)
        expected = self.width * self.height * self.bytes_per_pixel
        if len(immutable_pixels) != expected:
            raise ValueError(
                "pixel byte count does not match tightly packed raster geometry"
            )
        object.__setattr__(self, "pixels", immutable_pixels)

    @property
    def row_bytes(self) -> int:
        return self.width * self.bytes_per_pixel

    def is_comparable_to(self, other: FinalRaster) -> bool:
        if not isinstance(other, FinalRaster):
            return False
        return (
            self.width == other.width
            and self.height == other.height
            and self.bytes_per_pixel == other.bytes_per_pixel
            and self.pixel_format == other.pixel_format
        )


@dataclass(frozen=True, slots=True, order=True)
class RasterDamageRect:
    """One positive-area, half-open rectangle in final-raster pixels."""

    left: int
    top: int
    right: int
    bottom: int

    def __post_init__(self) -> None:
        for name in ("left", "top", "right", "bottom"):
            value = getattr(self, name)
            if isinstance(value, bool):
                raise TypeError(f"{name} must be an integer, not bool")
            try:
                normalized = operator.index(value)
            except TypeError as exc:
                raise TypeError(f"{name} must be an integer") from exc
            if normalized < 0:
                raise ValueError(f"{name} cannot be negative")
            object.__setattr__(self, name, int(normalized))
        if self.left >= self.right or self.top >= self.bottom:
            raise ValueError("damage rectangle must have positive area")


@dataclass(frozen=True, slots=True)
class RasterDamageGrid:
    """Caller-selected damage granularity, independent of panel policy."""

    tile_width: int = 1
    tile_height: int = 1

    def __post_init__(self) -> None:
        object.__setattr__(
            self, "tile_width", _positive_integer("tile_width", self.tile_width)
        )
        object.__setattr__(
            self,
            "tile_height",
            _positive_integer("tile_height", self.tile_height),
        )


@dataclass(frozen=True, slots=True)
class FinalRasterToken:
    """Exact cadence offer and immutable composite scope owning a raster."""

    offer_id: int
    scope: DisplayScope

    def __post_init__(self) -> None:
        object.__setattr__(
            self, "offer_id", _positive_integer("offer_id", self.offer_id)
        )
        if not isinstance(self.scope, DisplayScope):
            raise TypeError("scope must be DisplayScope")


@dataclass(frozen=True, slots=True)
class FinalRasterOffer:
    """Exact state pinned from refresh start through physical settlement."""

    token: FinalRasterToken
    raster: FinalRaster
    damage: tuple[RasterDamageRect, ...]
    hit_map: tuple[object, ...]

    def __post_init__(self) -> None:
        if not isinstance(self.token, FinalRasterToken):
            raise TypeError("token must be FinalRasterToken")
        if not isinstance(self.raster, FinalRaster):
            raise TypeError("raster must be FinalRaster")
        damage = tuple(self.damage)
        if any(not isinstance(rect, RasterDamageRect) for rect in damage):
            raise TypeError("damage must contain only RasterDamageRect values")
        object.__setattr__(self, "damage", damage)
        object.__setattr__(self, "hit_map", tuple(self.hit_map))


def derive_raster_damage(
    baseline: FinalRaster | None,
    candidate: FinalRaster,
    *,
    grid: RasterDamageGrid = RasterDamageGrid(),
) -> tuple[RasterDamageRect, ...]:
    """Compare exact final pixels and coalesce changed grid cells into rectangles.

    The grid is selected by the sink.  A 1x1 grid reports exact pixel damage;
    a controller-aligned grid conservatively expands each changed pixel to its
    tile.  Horizontal runs are joined, then identical runs in adjacent tile
    rows are joined vertically.  No application or semantic geometry enters
    this calculation.
    """

    if baseline is not None and not isinstance(baseline, FinalRaster):
        raise TypeError("baseline must be FinalRaster or None")
    if not isinstance(candidate, FinalRaster):
        raise TypeError("candidate must be FinalRaster")
    if not isinstance(grid, RasterDamageGrid):
        raise TypeError("grid must be RasterDamageGrid")
    if baseline is None or not baseline.is_comparable_to(candidate):
        return (
            RasterDamageRect(0, 0, candidate.width, candidate.height),
        )
    if baseline.pixels == candidate.pixels:
        return ()

    width = candidate.width
    height = candidate.height
    bytes_per_pixel = candidate.bytes_per_pixel
    row_bytes = candidate.row_bytes
    tile_width = grid.tile_width
    tile_height = grid.tile_height
    old = memoryview(baseline.pixels)
    new = memoryview(candidate.pixels)

    completed: list[RasterDamageRect] = []
    active: dict[tuple[int, int], RasterDamageRect] = {}

    for top in range(0, height, tile_height):
        bottom = min(height, top + tile_height)
        spans: list[tuple[int, int]] = []
        run_left: int | None = None
        run_right = 0

        for left in range(0, width, tile_width):
            right = min(width, left + tile_width)
            changed = False
            byte_left = left * bytes_per_pixel
            byte_right = right * bytes_per_pixel
            for row in range(top, bottom):
                start = row * row_bytes + byte_left
                end = row * row_bytes + byte_right
                if old[start:end] != new[start:end]:
                    changed = True
                    break
            if changed:
                if run_left is None:
                    run_left = left
                run_right = right
            elif run_left is not None:
                spans.append((run_left, run_right))
                run_left = None
        if run_left is not None:
            spans.append((run_left, run_right))

        next_active: dict[tuple[int, int], RasterDamageRect] = {}
        for left, right in spans:
            key = (left, right)
            prior = active.pop(key, None)
            if prior is None:
                next_active[key] = RasterDamageRect(left, top, right, bottom)
            else:
                next_active[key] = RasterDamageRect(
                    left,
                    prior.top,
                    right,
                    bottom,
                )
        completed.extend(active.values())
        active = next_active

    completed.extend(active.values())
    completed.sort(key=lambda rect: (rect.top, rect.left, rect.bottom, rect.right))
    return tuple(completed)


class FinalRasterDisplayState:
    """Retain one ACK baseline and one exact in-flight physical refresh.

    Logical pending/coalesced views intentionally remain the responsibility of
    :class:`DisplayCadenceScheduler`.  ``offer`` therefore rejects a second
    outstanding raster.  ``acknowledge`` is the only operation that advances
    the pixel baseline and its hit map.  A physical adapter may call it only
    after the controller's BUSY-to-READY completion and any required settling
    interval; refresh-command acceptance, BUSY, DMA completion, or an early
    READY sample must leave the offer outstanding.  This panel-neutral state
    intentionally does not simulate or infer controller signals.
    """

    def __init__(self, *, grid: RasterDamageGrid = RasterDamageGrid()) -> None:
        if not isinstance(grid, RasterDamageGrid):
            raise TypeError("grid must be RasterDamageGrid")
        self._grid = grid
        self._acknowledged: FinalRasterOffer | None = None
        self._offered: FinalRasterOffer | None = None

    @property
    def acknowledged(self) -> FinalRasterOffer | None:
        return self._acknowledged

    @property
    def offered(self) -> FinalRasterOffer | None:
        return self._offered

    def offer(
        self,
        token: FinalRasterToken,
        raster: FinalRaster,
        hit_map=(),
    ) -> FinalRasterOffer:
        """Pin one post-composition raster against the physical ACK baseline."""

        if not isinstance(token, FinalRasterToken):
            raise TypeError("token must be FinalRasterToken")
        if not isinstance(raster, FinalRaster):
            raise TypeError("raster must be FinalRaster")
        if self._offered is not None:
            raise RuntimeError("a final raster is already awaiting physical ACK")
        baseline = (
            None
            if self._acknowledged is None
            else self._acknowledged.raster
        )
        offered = FinalRasterOffer(
            token=token,
            raster=raster,
            damage=derive_raster_damage(baseline, raster, grid=self._grid),
            hit_map=tuple(hit_map),
        )
        self._offered = offered
        return offered

    def acknowledge(self, offered: FinalRasterOffer) -> None:
        """Promote the exact offer after the adapter proves sink completion.

        For physical e-paper that proof is controller BUSY-to-READY followed
        by the panel's required settling interval.  Successful SDL flip is a
        separate synchronous reference-sink boundary and does not call here.
        """

        if not isinstance(offered, FinalRasterOffer):
            raise TypeError("offered must be FinalRasterOffer")
        if offered is not self._offered:
            raise RuntimeError("raster is not the exact outstanding offer")
        self._offered = None
        self._acknowledged = offered

    def revoke(self, offered: FinalRasterOffer) -> None:
        """Drop one failed refresh without changing the physical baseline."""

        if not isinstance(offered, FinalRasterOffer):
            raise TypeError("offered must be FinalRasterOffer")
        if offered is not self._offered:
            raise RuntimeError("raster is not the exact outstanding offer")
        self._offered = None

    def replace_sink(self) -> None:
        """Forget all pixels when physical display ownership itself changes."""

        self._offered = None
        self._acknowledged = None


__all__ = [
    "FinalRaster",
    "FinalRasterDisplayState",
    "FinalRasterOffer",
    "FinalRasterToken",
    "RasterDamageGrid",
    "RasterDamageRect",
    "derive_raster_damage",
]
