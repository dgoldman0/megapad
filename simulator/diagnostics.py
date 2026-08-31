"""Deterministic pseudo-BIOS diagnostics for the hosted profile.

The source simulator has no MP64 pipeline, cache, tile datapath, or destructive
RAM test engine.  It still provides the safe public diagnostic observations so
ordinary KDOS can load unchanged.  ``PERF-CYCLES`` is backend-local semantic
work rather than machine cycles; unsafe or unsupported active tests fail
explicitly instead of manufacturing a hardware pass.
"""

from __future__ import annotations

from shared.cells import MASK64, u64
from simulator.errors import ExecutionError
from simulator.tile import (
    TILE_BYTES,
    tile_add_u8,
    tile_dot_u8,
    tile_multiply_u8,
    tile_sum_u8,
)


BIST_IDLE = 0
BIST_RUNNING = 1
BIST_PASS = 2
BIST_FAIL = 3

TILE_IDLE = 0
TILE_RUNNING = 1
TILE_PASS = 2
TILE_FAIL = 3


class HostedDiagnosticsService:
    """Own the observable diagnostic state of one hosted full-core profile."""

    __slots__ = (
        "_bist_fail_address",
        "_bist_fail_data",
        "_bist_status",
        "_icache_enabled",
        "_icache_hits",
        "_icache_misses",
        "_perf_cycles",
        "_perf_enabled",
        "_perf_extmem",
        "_perf_stalls",
        "_perf_tileops",
        "_tile_detail",
        "_tile_status",
    )

    def __init__(
        self,
        *,
        perf_cycles: int = 0,
        bist_status: int = BIST_IDLE,
        bist_fail_address: int = 0,
        bist_fail_data: int = 0,
    ) -> None:
        self._perf_cycles = self._require_cell(
            perf_cycles,
            label="performance work counter",
        )
        self._perf_enabled = True
        self._perf_stalls = 0
        self._perf_tileops = 0
        self._perf_extmem = 0
        if isinstance(bist_status, bool) or not isinstance(bist_status, int):
            raise TypeError("BIST status must be an integer")
        if bist_status not in (BIST_IDLE, BIST_RUNNING, BIST_PASS, BIST_FAIL):
            raise ValueError("BIST status must be 0, 1, 2, or 3")
        self._bist_status = bist_status
        self._bist_fail_address = self._require_cell(
            bist_fail_address,
            label="BIST failure address",
        )
        self._bist_fail_data = self._require_cell(
            bist_fail_data,
            label="BIST failure data",
        )
        self._tile_status = TILE_IDLE
        self._tile_detail = 0
        self._icache_enabled = True
        self._icache_hits = 0
        self._icache_misses = 0

    @property
    def perf_cycles(self) -> int:
        return self._perf_cycles

    @property
    def perf_stalls(self) -> int:
        return self._perf_stalls

    @property
    def perf_tileops(self) -> int:
        return self._perf_tileops

    @property
    def perf_extmem(self) -> int:
        return self._perf_extmem

    def account_work(self) -> None:
        """Count one admitted semantic dispatcher unit with cell wrapping."""

        if self._perf_enabled:
            self._perf_cycles = u64(self._perf_cycles + 1)

    def account_tile_operation(self) -> None:
        """Count one completed hosted tile operation with cell wrapping."""

        if self._perf_enabled:
            self._perf_tileops = u64(self._perf_tileops + 1)

    def clone(self) -> HostedDiagnosticsService:
        """Return an independent copy suitable for one runtime instance."""

        clone = HostedDiagnosticsService(
            perf_cycles=self._perf_cycles,
            bist_status=self._bist_status,
            bist_fail_address=self._bist_fail_address,
            bist_fail_data=self._bist_fail_data,
        )
        clone._perf_enabled = self._perf_enabled
        clone._perf_stalls = self._perf_stalls
        clone._perf_tileops = self._perf_tileops
        clone._perf_extmem = self._perf_extmem
        clone._tile_status = self._tile_status
        clone._tile_detail = self._tile_detail
        clone._icache_enabled = self._icache_enabled
        clone._icache_hits = self._icache_hits
        clone._icache_misses = self._icache_misses
        return clone

    def reset_performance(self) -> None:
        self._perf_cycles = 0
        self._perf_stalls = 0
        self._perf_tileops = 0
        self._perf_extmem = 0
        self._perf_enabled = True

    @property
    def bist_status(self) -> int:
        return self._bist_status

    @property
    def bist_fail_address(self) -> int:
        return self._bist_fail_address

    @property
    def bist_fail_data(self) -> int:
        return self._bist_fail_data

    def run_full_bist(self) -> None:
        raise ExecutionError(
            "BIST-FULL is destructive and unavailable in the hosted runtime"
        )

    def run_quick_bist(self) -> None:
        raise ExecutionError(
            "BIST-QUICK is destructive and unavailable in the hosted runtime"
        )

    @property
    def tile_status(self) -> int:
        return self._tile_status

    @property
    def tile_detail(self) -> int:
        return self._tile_detail

    def run_tile_test(self) -> None:
        self._tile_status = TILE_RUNNING
        self._tile_detail = 0
        indexes = bytes(range(TILE_BYTES))
        try:
            if tile_add_u8(indexes, bytes((100,)) * TILE_BYTES) != bytes(
                range(100, 100 + TILE_BYTES)
            ):
                self._tile_detail |= 0x1
        except Exception:
            self._tile_detail |= 0x1
        try:
            if tile_multiply_u8(indexes, bytes((3,)) * TILE_BYTES) != bytes(
                (index * 3) & 0xFF for index in range(TILE_BYTES)
            ):
                self._tile_detail |= 0x2
        except Exception:
            self._tile_detail |= 0x2
        try:
            ones = bytes((1,)) * TILE_BYTES
            if tile_dot_u8(ones, ones) != TILE_BYTES:
                self._tile_detail |= 0x4
        except Exception:
            self._tile_detail |= 0x4
        try:
            if tile_sum_u8(bytes((2,)) * TILE_BYTES) != 2 * TILE_BYTES:
                self._tile_detail |= 0x8
        except Exception:
            self._tile_detail |= 0x8
        self._tile_status = TILE_FAIL if self._tile_detail else TILE_PASS

    @property
    def icache_enabled(self) -> bool:
        return self._icache_enabled

    @property
    def icache_hits(self) -> int:
        return self._icache_hits

    @property
    def icache_misses(self) -> int:
        return self._icache_misses

    def enable_icache(self) -> None:
        self._icache_enabled = True

    def disable_icache(self) -> None:
        self._icache_enabled = False

    def invalidate_icache(self) -> None:
        # BIOS writes control value 3, which both invalidates and enables.
        self._icache_enabled = True
        self._icache_hits = 0
        self._icache_misses = 0

    @staticmethod
    def _require_cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be a uint64 integer")
        if not 0 <= value <= MASK64:
            raise ValueError(f"{label} must be a uint64 integer")
        return value


__all__ = [
    "BIST_FAIL",
    "BIST_IDLE",
    "BIST_PASS",
    "BIST_RUNNING",
    "HostedDiagnosticsService",
    "TILE_FAIL",
    "TILE_IDLE",
    "TILE_PASS",
    "TILE_RUNNING",
]
