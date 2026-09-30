"""Fixed ordinary-region storage shared by execution-engine adapters.

This owner knows guest geometry and buffer lifetime, not MMIO, execution or
hybrid transitions. It allocates each region separately, so address gaps cost
no storage and two physical regions cannot alias the same bytes.
"""

from __future__ import annotations

from collections.abc import Iterable

from shared.cells import MASK64


class DenseMemoryBacking:
    """Own zero-filled, fixed-capacity buffers for nonoverlapping regions.

    Private memoryview pins prevent exporter resizing throughout this owner's
    lifetime. Clients receive independent views; releasing one cannot release
    the owner's pin. Adapters retaining their views or native buffer leases
    also keep the bytes alive after the owner is otherwise unreferenced.
    Callers serialize access; the backing owner does not schedule execution.
    """

    __slots__ = ("_regions", "_pins")

    def __init__(self, regions: Iterable[tuple[int, int]]) -> None:
        geometry = []
        for base, size in regions:
            if (not isinstance(base, int) or isinstance(base, bool)
                    or not isinstance(size, int) or isinstance(size, bool)):
                raise TypeError("region base and size must be integers")
            if not 0 <= base <= MASK64 or size <= 0 or size - 1 > MASK64 - base:
                raise ValueError("region must be a nonempty uint64 span")
            geometry.append((base, size))
        geometry.sort()
        for (base, size), (next_base, _next_size) in zip(geometry, geometry[1:]):
            if base + size > next_base:
                raise ValueError("ordinary regions must not overlap")
        # Validate the entire geometry before allocating any backing.
        self._regions = tuple(geometry)
        self._pins = {base: memoryview(bytearray(size)) for base, size in geometry}

    @property
    def regions(self) -> tuple[tuple[int, int], ...]:
        """Immutable sorted ``(base, size)`` geometry."""

        return self._regions

    def buffer_at(self, base: int) -> memoryview:
        """Return an independent writable byte view of one exact region."""

        return memoryview(self._pins[base])


__all__ = ["DenseMemoryBacking"]
