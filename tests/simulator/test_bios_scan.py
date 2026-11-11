"""The hosted SCAN, against the BIOS word's contract.

``SCAN ( c-addr u char -- c-addr' u' )`` steps to the first byte of a string
equal to ``char``.  KDOS finds the end of each source line with it.  The
emulator's tests run the same cases through ``bios.asm``
(``tests/test_system.py::TestBIOSScan``).
"""

from __future__ import annotations

import pytest

from simulator.memory import EXTERNAL_BASE, MemoryAccessError
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime

EXTERNAL_BYTES = 1 << 20
TEXT = b"ab\ncd\n\n"


def _runtime() -> tuple[MegaForthRuntime, int]:
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(external_size=EXTERNAL_BYTES)
    )
    runtime.memory.write_bytes(EXTERNAL_BASE, TEXT)
    return runtime, EXTERNAL_BASE


def _scan(
    runtime: MegaForthRuntime, address: int, count: int, char: int
) -> tuple[int, ...]:
    data = runtime.main_context.data
    assert data.snapshot() == ()
    data.push(address)
    data.push(count % 2**64)
    data.push(char)
    runtime.execute("SCAN")
    result = data.snapshot()
    data.clear()
    return result


def test_scan_finds_the_first_match_and_keeps_the_rest() -> None:
    runtime, base = _runtime()

    assert _scan(runtime, base, 5, 10) == (base + 2, 3)
    assert _scan(runtime, base, 5, ord("a")) == (base, 5)
    assert _scan(runtime, base, 5, ord("d")) == (base + 4, 1)
    assert _scan(runtime, base + 3, 3, 10) == (base + 5, 1)


def test_scan_without_a_match_ends_at_the_strings_end() -> None:
    """The count bounds the scan: a match just past it is not found."""

    runtime, base = _runtime()

    assert _scan(runtime, base, 2, 10) == (base + 2, 0)
    assert _scan(runtime, base, 6, ord("x")) == (base + 6, 0)


def test_scan_leaves_an_empty_or_negative_count_as_it_is() -> None:
    runtime, base = _runtime()

    assert _scan(runtime, base, 0, 10) == (base, 0)
    assert _scan(runtime, base, -3, 10) == (base, (-3) % 2**64)


def test_scan_compares_one_byte_of_the_character() -> None:
    runtime, base = _runtime()

    # 0x10A: its low byte is the LF.
    assert _scan(runtime, base, 5, 0x10A) == (base + 2, 3)


@pytest.mark.parametrize("position", [0, 1, 254, 255, 256, 257, 511, 512, 699])
def test_scan_finds_a_match_anywhere_in_a_long_string(position: int) -> None:
    """The hosted word reads in chunks; a match is found on either side of
    each chunk's edge."""

    runtime, base = _runtime()
    text = bytearray(b"x" * 700)
    text[position] = 10
    runtime.memory.write_bytes(base + 64, bytes(text))

    assert _scan(runtime, base + 64, 700, 10) == (base + 64 + position, 700 - position)
    assert _scan(runtime, base + 64, position, 10) == (base + 64 + position, 0)


def test_scan_reads_no_further_than_the_first_match() -> None:
    """A string whose count runs past the end of memory is scanned as the
    BIOS scans it, a byte at a time: a match before the end is found, and a
    scan that reaches the end faults there."""

    runtime, base = _runtime()
    end = base + EXTERNAL_BYTES
    runtime.memory.write_bytes(end - 3, b"ab\n")

    assert _scan(runtime, end - 3, 100, 10) == (end - 1, 98)
    with pytest.raises(MemoryAccessError):
        _scan(runtime, end - 3, 100, ord("q"))
    runtime.main_context.data.clear()
