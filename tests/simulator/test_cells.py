"""Focused tests for backend-neutral 64-bit cell semantics."""

from __future__ import annotations

from shared.cells import (
    CELL_BITS,
    CELL_BYTES,
    FALSE,
    MASK64,
    SIGN64,
    TRUE,
    forth_flag,
    s64,
    u64,
)


def test_cell_constants_pin_the_public_representation() -> None:
    assert CELL_BITS == 64
    assert CELL_BYTES == 8
    assert MASK64 == 0xFFFF_FFFF_FFFF_FFFF
    assert SIGN64 == 0x8000_0000_0000_0000
    assert FALSE == 0
    assert TRUE == MASK64


def test_u64_reduces_unbounded_and_negative_integers() -> None:
    assert u64(0) == 0
    assert u64(MASK64) == MASK64
    assert u64(MASK64 + 1) == 0
    assert u64((1 << 130) + 7) == 7
    assert u64(-1) == MASK64
    assert u64(-(1 << 64) - 2) == MASK64 - 1


def test_s64_interprets_the_normalized_cell_pattern() -> None:
    assert s64(0) == 0
    assert s64(SIGN64 - 1) == (1 << 63) - 1
    assert s64(SIGN64) == -(1 << 63)
    assert s64(MASK64) == -1
    assert s64(1 << 64) == 0
    assert s64((1 << 64) + SIGN64 + 9) == -(1 << 63) + 9


def test_forth_flag_uses_canonical_full_width_truth() -> None:
    assert forth_flag(False) == FALSE
    assert forth_flag(True) == TRUE
