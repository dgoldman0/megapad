"""Native CRC words against the Python dispatcher.

CRC-MODE!, CRC-RESET, CRC-INIT!, CRC-FEED, CRC-FEED-BYTE, CRC@,
CRC-RAW-FINAL@ and CRC-FINAL@ run in the native executor when the runtime
keeps the CRC service the BIOS words were built over.  The service is native
code that the Python words call too, and its recurrence is
shared/accel/crc.h, the emulator's CRC unit's own.  Each case compares the
complete observation of the Python and native executors with the service's
mode, accumulator and owner.
"""

from __future__ import annotations

import pytest


pytest.importorskip("_megaforth_native")

from shared.crypto_caps import CRYPTO_CAP_CRC_REFLECT_RAW  # noqa: E402
from simulator.platform import create_address_space  # noqa: E402
from simulator.runtime import MegaForthRuntime  # noqa: E402
from tests.simulator.test_native_execution import _observe, _runtimes  # noqa: E402

_SETUP = b"CREATE CRC-BUF 64 ALLOT : FILL-BUF 64 0 DO I 37 * 11 + CRC-BUF I + C! LOOP ; FILL-BUF "


def _crc(runtime):
    service = runtime.crc
    return service.mode, service.accumulator, service.owner


def _compare(source: bytes, *, runtimes=None, owner=None, require_native=True):
    if runtimes is None:
        runtimes = _runtimes(_SETUP + source)
    if owner is not None:
        for runtime in runtimes:
            assert runtime.crc.select_mode(owner, 1) == 0
    observed = [_observe(runtime, "RUN") for runtime in runtimes]
    assert observed[1][0] == observed[0][0]
    assert _crc(runtimes[1]) == _crc(runtimes[0])
    if require_native:
        assert observed[1][1] > 0, "the CRC words must run natively"
    return observed[0][0], runtimes


@pytest.mark.parametrize("mode", (0, 1, 2, 4, 5, 6))
def test_every_mode_feeds_cells_and_bytes_as_the_reference(mode: int) -> None:
    observation, runtimes = _compare(
        b": RUN " + str(mode).encode() + b" CRC-MODE! CRC-RESET "
        b"8 0 DO CRC-BUF I CELLS + @ CRC-FEED DROP LOOP "
        b"0x1FF CRC-FEED-BYTE 0xA5 CRC-FEED-BYTE CRC@ CRC-FINAL@ ;"
    )
    assert observation["error"] is None
    assert runtimes[0].crc.owner is None


def test_seeds_raw_finals_and_refetches_match() -> None:
    observation, _ = _compare(
        b": RUN 5 CRC-MODE! 0x123456789ABCDEF0 CRC-INIT! "
        b"CRC-BUF 1+ @ CRC-FEED CRC@ CRC-RAW-FINAL@ "
        b"2 CRC-MODE! CRC-RESET CRC-BUF @ CRC-FEED "
        b"5 CRC-MODE! CRC@ CRC-FINAL@ ;"
    )
    assert observation["error"] is None


def test_an_accumulator_keeps_its_high_word_across_a_mode_change() -> None:
    # CRC-MODE! does not touch the accumulator, so a fetch after a change
    # from a 64-bit mode shows all of it.
    observation, _ = _compare(
        b": RUN 6 CRC-MODE! CRC-RESET CRC-BUF @ CRC-FEED CRC-FINAL@ DROP "
        b"1 CRC-MODE! CRC@ CRC-BUF @ CRC-FEED CRC@ CRC-FINAL@ ;"
    )
    assert observation["error"] is None


def test_refused_modes_and_words_without_the_engine_report_their_status() -> None:
    observation, runtimes = _compare(
        b": RUN 3 CRC-MODE! 7 CRC-MODE! -1 CRC-MODE! CRC-RESET 9 CRC-INIT! "
        b"5 CRC-FEED 5 CRC-FEED-BYTE CRC@ CRC-RAW-FINAL@ CRC-FINAL@ "
        b"1 CRC-MODE! 2 CRC-MODE! ;"
    )
    assert observation["error"] is None
    assert observation["data"][:3] == (3, 3, 3)
    assert runtimes[0].crc.owner == (0, 0)


def test_an_engine_another_identity_holds_refuses_every_word() -> None:
    observation, runtimes = _compare(
        b": RUN 1 CRC-MODE! CRC-RESET 9 CRC-INIT! 5 CRC-FEED 5 CRC-FEED-BYTE "
        b"CRC@ CRC-RAW-FINAL@ CRC-FINAL@ ;",
        owner=(1, 7),
    )
    assert observation["error"] is None
    assert observation["data"] == (2, 2, 2, 2, 2, 0, 2, 0, 2, 0)
    assert runtimes[1].crc.owner == (1, 7)


def test_without_reflected_modes_raw_fetch_still_releases_a_normal_mode() -> None:
    capabilities = 0
    runtimes = []
    for backend in ("python", "native"):
        runtime = MegaForthRuntime(
            execution_backend=backend,
            memory=create_address_space(crypto_capabilities=capabilities),
        )
        runtime.evaluate(
            _SETUP + b": RUN 5 CRC-MODE! 1 CRC-MODE! CRC-RESET CRC-BUF @ CRC-FEED "
            b"CRC-RAW-FINAL@ CRC@ ;",
            source_name="crc.f",
        )
        runtimes.append(runtime)
    assert not runtimes[1].crc.capabilities & CRYPTO_CAP_CRC_REFLECT_RAW
    observation, _ = _compare(b"", runtimes=runtimes)
    assert observation["error"] is None
    assert runtimes[0].crc.owner is None


def test_a_replaced_crc_service_keeps_the_words_in_python() -> None:
    runtimes = _runtimes(_SETUP + b": RUN 5 CRC-MODE! CRC-RESET 7 CRC-FEED CRC-FINAL@ ;")
    native = runtimes[1]
    from simulator.crc import HostedCRCService

    class Observed(HostedCRCService):
        fed = 0

        def feed_cell(self, identity, cell):
            type(self).fed += 1
            return super().feed_cell(identity, cell)

    native.crc = Observed(native.crc.capabilities)
    observation, _ = _observe(native, "RUN")
    assert observation["error"] is None
    assert Observed.fed == 1
    assert native.crc.owner is None
