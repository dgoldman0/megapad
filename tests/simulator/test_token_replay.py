"""The hosted TOKEN-REPLAY, against the BIOS word's contract.

``TOKEN-REPLAY ( rec-addr rec-len tab-addr tab-slots -- )`` runs a recorded
token stream through the interpreter's own two steps.  The emulator's tests
run the same streams through ``bios.asm``
(``tests/test_system.py::TestBIOSTokenReplay``), and both read their streams
from ``tests/token_streams.py``.
"""

from __future__ import annotations

import pytest

from shared.token_form import TokenWriter
from simulator.memory import EXTERNAL_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.token_streams import STREAMS

TABLE = EXTERNAL_BASE + 0x10000
REPORT = (
    ' CR ." status=" EVAL-STATUS @ . ." depth=" DEPTH . ." line=" EVAL-LINE @ .'
)


def _replay(records: bytes, slots: int, before: str = "") -> str:
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(external_size=1 << 20)
    )
    runtime.memory.write_bytes(EXTERNAL_BASE, records)
    if before:
        runtime.evaluate(before.encode())
    start = len(runtime.uart_output)
    runtime.evaluate(
        f"{EXTERNAL_BASE} {len(records)} {TABLE} {slots} TOKEN-REPLAY{REPORT}".encode()
    )
    return bytes(runtime.uart_output[start:]).decode("latin-1")


@pytest.mark.parametrize("name", sorted(STREAMS))
def test_a_stream_replays_as_the_bios_word_does(name: str) -> None:
    make, expected = STREAMS[name]
    records, slots = make()

    text = _replay(records, slots)

    assert " ".join(text.split()) == expected


def test_the_table_holds_each_found_word_in_order() -> None:
    writer = TokenWriter()
    writer.number(1)
    writer.word("dup", b"DUP")
    writer.word("drop", b"DROP")
    writer.word("dup", b"DUP")
    writer.word("dot", b".")
    writer.word("dot", b".")
    records = writer.finish()
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(external_size=1 << 20)
    )
    runtime.memory.write_bytes(EXTERNAL_BASE, records)
    runtime.memory.fill(TABLE, 8 * 8, 0xEE)

    runtime.evaluate(
        f"{EXTERNAL_BASE} {len(records)} {TABLE} 8 TOKEN-REPLAY".encode()
    )

    cells = [runtime.memory.read64(TABLE + 8 * index) for index in range(4)]
    assert cells[:3] == [
        runtime.find(name).header_address for name in (b"DUP", b"DROP", b".")
    ]
    # The slot after the last import is not written.
    assert cells[3] == 0xEEEEEEEEEEEEEEEE


def test_a_replay_nested_under_a_failed_evaluation_runs_nothing() -> None:
    """As EVALUATE: at a nonzero depth with a status set, nothing runs and
    the four arguments are dropped."""

    writer = TokenWriter()
    writer.number(7)
    writer.word("dot", b".")
    records = writer.finish()
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(external_size=1 << 20)
    )
    runtime.memory.write_bytes(EXTERNAL_BASE, records)
    runtime.evaluate(
        f": NESTED  1 EVAL-STATUS !  {EXTERNAL_BASE} {len(records)} {TABLE} 4 "
        "TOKEN-REPLAY ;".encode()
    )
    start = len(runtime.uart_output)

    runtime.evaluate(b'S" NESTED" EVALUATE  CR ." depth=" DEPTH .')

    text = bytes(runtime.uart_output[start:]).decode("latin-1")
    assert "7" not in text.split("depth=")[0]
    assert "depth=0" in text


def test_a_word_that_unlinks_the_dictionary_ends_the_replay() -> None:
    """The table holds entry addresses, which stand only while no entry
    leaves the dictionary."""

    writer = TokenWriter()
    writer.word("here", b"HERE")
    writer.word("latest", b"LATEST")
    writer.word("rollback", b"DICT-ROLLBACK")
    writer.number(5)
    writer.word("dot", b".")
    records = writer.finish()

    text = _replay(records, writer.slots)

    assert "does not match" in text
    assert "status=7" in text
    assert " 5 " not in f" {text} ".split("status=")[0]
