"""Focused BIOS words required by KDOS MARKER and FORGET."""

from __future__ import annotations

import pytest

from simulator.errors import ExecutionError
from simulator.runtime import MegaForthRuntime


def test_latest_tracks_the_newest_live_dictionary_header() -> None:
    runtime = MegaForthRuntime()
    previous = runtime.dictionary.latest
    assert previous != 0

    runtime.execute("LATEST")
    assert runtime.main_context.data.pop() == previous

    definition = runtime.evaluate(b": SNAPSHOT-TARGET 17 ;")
    target, = definition.definitions
    runtime.execute("LATEST")
    assert runtime.main_context.data.snapshot() == (target.header_address,)


def test_dict_rollback_consumes_a_valid_numeric_pair_after_publication() -> None:
    runtime = MegaForthRuntime()
    context = runtime.main_context
    retained = runtime.evaluate(b": RETAINED 11 ;").definitions[0]
    saved_here = runtime.dictionary.here
    transient = runtime.evaluate(b": TRANSIENT 22 ;").definitions[0]
    active_here = runtime.dictionary.here
    stale_bytes = runtime.memory.read_bytes(saved_here, active_here - saved_here)
    context.data.push(saved_here)
    context.data.push(retained.header_address)

    runtime.execute("DICT-ROLLBACK")

    assert context.data.snapshot() == ()
    assert runtime.dictionary.here == saved_here
    assert runtime.dictionary.latest_word is retained
    assert runtime.find("RETAINED") is retained
    assert runtime.find("TRANSIENT") is None
    with pytest.raises(KeyError):
        runtime.dictionary.resolve(transient.xt)
    assert runtime.memory.read_bytes(saved_here, len(stale_bytes)) == stale_bytes


def test_dict_rollback_rejects_an_invalid_pair_without_consuming_it() -> None:
    runtime = MegaForthRuntime()
    context = runtime.main_context
    active_here = runtime.dictionary.here
    active_latest = runtime.dictionary.latest
    context.data.push(active_here + 1)
    context.data.push(active_latest)

    with pytest.raises(ExecutionError, match="saved HERE is ahead"):
        runtime.execute("DICT-ROLLBACK")

    assert context.data.snapshot() == (active_here + 1, active_latest)
    assert runtime.dictionary.here == active_here
    assert runtime.dictionary.latest == active_latest


def test_dict_rollback_cannot_remove_the_semantic_bios_prefix() -> None:
    runtime = MegaForthRuntime()
    context = runtime.main_context
    core_word = runtime.find("DUP")
    assert core_word is not None
    prior_header = runtime.memory.read64(core_word.header_address)
    active_here = runtime.dictionary.here
    active_latest = runtime.dictionary.latest
    context.data.push(core_word.header_address)
    context.data.push(prior_header)

    with pytest.raises(ExecutionError, match="protected dictionary prefix"):
        runtime.execute("DICT-ROLLBACK")

    assert context.data.snapshot() == (core_word.header_address, prior_header)
    assert runtime.dictionary.here == active_here
    assert runtime.dictionary.latest == active_latest
    assert runtime.find("DUP") is core_word
    assert runtime.dictionary.resolve(core_word.xt) is core_word


def test_count_returns_the_payload_address_and_unsigned_length_byte() -> None:
    runtime = MegaForthRuntime()
    address = 0x70_000
    runtime.memory.write_bytes(address, b"\xffpayload")
    context = runtime.new_context()
    context.data.push(address)

    runtime.execute("COUNT", context=context)

    assert context.data.snapshot() == (address + 1, 0xFF)


@pytest.mark.parametrize(
    ("value", "expected"),
    (
        (ord("a"), ord("A")),
        (ord("m"), ord("M")),
        (ord("z"), ord("Z")),
        (ord("A"), ord("A")),
        (ord("0"), ord("0")),
        (0x161, 0x161),
    ),
)
def test_uchar_only_folds_lowercase_ascii_cells(value: int, expected: int) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(value)

    runtime.execute("UCHAR", context=context)

    assert context.data.snapshot() == (expected,)
