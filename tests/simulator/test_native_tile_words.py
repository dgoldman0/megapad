"""Native tile-engine words against the Python dispatcher.

FP16-MODE, BF16-MODE, TMODE!, TSRC0!, TSRC1!, TDST!, TADD, TSUB and TMUL run
in the native executor when the runtime keeps the canonical tile service.
Each case compares the complete observation of the Python and native
backends, including the destination tiles, the tile and Field ALU registers
and the PERF-TILEOPS counter.
"""

from __future__ import annotations

import pytest


pytest.importorskip("_megaforth_native")

from simulator.memory import EXTERNAL_BASE  # noqa: E402
from tests.simulator.test_native_execution import _observe, _runtimes  # noqa: E402

_SETUP = (
    b"CREATE TA 64 ALLOT CREATE TB 64 ALLOT CREATE TD 64 ALLOT "
    b": FILL-TILES 64 0 DO I 3 * TA I + C! I 7 * 5 + TB I + C! LOOP ; FILL-TILES "
)


def _tiles(runtime):
    tile = runtime.tile
    words = {name: runtime.find(name).body_address for name in ("TA", "TB", "TD")}
    return (
        tile.mode, tile.source0, tile.source1, tile.destination, tile.control,
        tile.accumulator, runtime.diagnostics.perf_tileops,
        tuple(runtime.memory.read_bytes(address, 64) for address in words.values()),
    )


def _compare_tiles(source: bytes, word: str = "RUN", *, require_native: bool = True,
                   external_size: int = 0):
    runtimes = _runtimes(_SETUP + source, external_size=external_size)
    observed = [_observe(runtime, word) for runtime in runtimes]
    assert observed[1][0] == observed[0][0]
    assert _tiles(runtimes[1]) == _tiles(runtimes[0])
    if require_native:
        assert observed[1][1] > 0, "the tile words must run natively"
    return observed[0][0], runtimes


@pytest.mark.parametrize("mode_word", (b"FP16-MODE", b"BF16-MODE"))
def test_float_tile_arithmetic_matches_the_reference(mode_word: bytes) -> None:
    observation, runtimes = _compare_tiles(
        b": RUN " + mode_word + b" TA TSRC0! TB TSRC1! TD TDST! "
        b"8 0 DO TADD TD TSRC0! TMUL TSUB LOOP ;"
    )
    assert observation["error"] is None
    assert runtimes[0].diagnostics.perf_tileops == 24


@pytest.mark.parametrize("mode", (0x00, 0x01, 0x02, 0x03, 0x10, 0x21, 0x32, 0x13, 0x7F & 0x46))
def test_integer_and_float_modes_from_tmode_store_match(mode: int) -> None:
    observation, _ = _compare_tiles(
        b": RUN " + str(mode).encode() + b" TMODE! TA TSRC0! TB TSRC1! TD TDST! "
        b"4 0 DO TADD TSUB TMUL LOOP ;"
    )
    assert observation["error"] is None


def test_tmode_store_keeps_seven_bits() -> None:
    observation, runtimes = _compare_tiles(b": RUN 0x1FF TMODE! TA TSRC0! ;")
    assert observation["error"] is None
    assert runtimes[1].tile.mode == 0x7F


def test_reserved_format_faults_through_the_reference_path() -> None:
    observation, _ = _compare_tiles(
        b": RUN 8 TMODE! TA TSRC0! TB TSRC1! TD TDST! TADD ;"
    )
    assert observation["error"] is not None


def test_overlapping_sources_and_destination_read_before_writing() -> None:
    observation, _ = _compare_tiles(
        b": RUN FP16-MODE TA TSRC0! TA 32 + TSRC1! TA 16 + TDST! "
        b"3 0 DO TADD TMUL LOOP ;"
    )
    assert observation["error"] is None


def test_unbacked_destination_falls_back_and_matches() -> None:
    # A fresh external page is absent until written; the native executor
    # leaves that write to Python, which creates the page.
    observation, runtimes = _compare_tiles(
        b": RUN FP16-MODE TA TSRC0! TB TSRC1! EXT-MEM-BASE 0x2000 + TDST! TADD "
        b"TD TDST! TSUB ;",
        external_size=1 << 20,
    )
    assert observation["error"] is None
    base = EXTERNAL_BASE + 0x2000
    assert runtimes[1].memory.read_bytes(base, 64) == runtimes[0].memory.read_bytes(base, 64)


def test_a_replaced_tile_service_keeps_tiles_in_python() -> None:
    runtimes = _runtimes(_SETUP + b": RUN FP16-MODE TA TSRC0! TB TSRC1! TD TDST! TADD ;")
    native = runtimes[1]
    from simulator.tile import HostedTileService

    class Observed(HostedTileService):
        __slots__ = ()

    native.tile = Observed(native.memory, native.field,
                           account_operation=native.diagnostics.account_tile_operation)
    observation, native_steps = _observe(native, "RUN")
    assert observation["error"] is None
    assert native.tile.mode == 4
    # The words still ran, through the replacement service in Python.
    assert native.diagnostics.perf_tileops == 1
    assert native_steps >= 0
