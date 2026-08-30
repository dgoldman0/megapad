"""First unchanged Akashic source acceptance for the hosted runtime."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import FALSE, TRUE
from simulator.runtime import MegaForthRuntime


FIXTURE = Path(__file__).with_name("fixtures") / "uint-range.f"
FIXTURE_SHA256 = "11b9b0d2a87466aec24b1952f921226f4ae4681e396a677136a5d17152b103e8"


@pytest.fixture
def runtime() -> MegaForthRuntime:
    source = FIXTURE.read_bytes()
    assert hashlib.sha256(source).hexdigest() == FIXTURE_SHA256
    hosted = MegaForthRuntime()
    result = hosted.evaluate(source, source_name="akashic/utils/uint-range.f")
    assert [word.name for word in result.definitions] == [
        b"URANGE-VALID?",
        b"URANGE-OVERLAP?",
    ]
    assert b"akashic-uint-range" in hosted.provided_modules
    return hosted


@pytest.mark.parametrize(
    ("start", "count", "expected"),
    (
        (1000, 16, TRUE),
        (1000, 0, TRUE),
        (1000, -1, FALSE),
        (-8, 7, TRUE),
        (-8, 8, FALSE),
        (-1, 0, TRUE),
    ),
)
def test_real_urange_valid_source_vectors(
    runtime: MegaForthRuntime,
    start: int,
    count: int,
    expected: int,
) -> None:
    context = runtime.new_context()
    context.data.push(start)
    context.data.push(count)

    runtime.execute("URANGE-VALID?", context=context)

    assert context.data.snapshot() == (expected,)
    assert context.returns.snapshot() == ()


@pytest.mark.parametrize(
    ("values", "expected"),
    (
        ((1000, 10, 1005, 5), (TRUE, TRUE)),
        ((1000, 10, 1010, 5), (FALSE, TRUE)),
        ((1000, 0, 1000, 1), (FALSE, TRUE)),
        ((1000, 1, 1001, 0), (FALSE, TRUE)),
        ((-16, 8, -12, 4), (TRUE, TRUE)),
        ((-8, 8, -4, 1), (FALSE, FALSE)),
        ((1000, -1, 1000, 1), (FALSE, FALSE)),
        ((1000, 1, 1000, -1), (FALSE, FALSE)),
        ((1000, 0, 1000, -1), (FALSE, FALSE)),
        ((1000, -1, 1000, 0), (FALSE, FALSE)),
    ),
)
def test_real_urange_overlap_source_vectors(
    runtime: MegaForthRuntime,
    values: tuple[int, int, int, int],
    expected: tuple[int, int],
) -> None:
    context = runtime.new_context()
    for value in values:
        context.data.push(value)

    runtime.execute("URANGE-OVERLAP?", context=context)

    assert context.data.snapshot() == expected
    assert context.returns.snapshot() == ()
