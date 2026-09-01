"""Unchanged-source acceptance for KDOS dictionary search."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import TRUE
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_documentation_browser import (
    _load_documentation_browser,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-dictionary-search-6428-6510.f"
)

FIRST_LINE = 6428
LAST_LINE = 6510
SLICE_BYTES = 2_682
SLICE_SHA256 = (
    "c1c7be64fd2d1c86465edec8f0fd6922c2742c6b77be9267dc7638f7eeb3ce5a"
)
SLICE_GIT_BLOB = "8335b7ef5566340e7fa1115de27fec9c75f6ae97"
HOSTED_DICTIONARY_GROWTH = 398
SEARCH_STEP_BUDGET = 1_000_000

SOURCE_LEDGER = (
    (":", b"ENTRY>LINK"),
    (":", b"ENTRY>NAME"),
    ("VARIABLE", b"IC-PA"),
    ("VARIABLE", b"IC-PL"),
    ("VARIABLE", b"IC-SA"),
    ("VARIABLE", b"IC-SL"),
    (":", b"ICONTAINS?"),
    ("VARIABLE", b"WL-CNT"),
    ("VARIABLE", b"WL-ENT"),
    ("VARIABLE", b"WL-PA"),
    ("VARIABLE", b"WL-PL"),
    (":", b"WORDS-LIKE"),
    (":", b"APROPOS"),
    (":", b".RECENT"),
)

DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)
ZERO_VARIABLES = tuple(
    name for definer, name in SOURCE_LEDGER if definer == "VARIABLE"
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\\ " + b"=" * 69 + b"\n"
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa78  Scheduler & Tasks\n"
    return source


def _evaluate_dictionary_search(
    runtime: MegaForthRuntime,
) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_dictionary_search(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_dictionary_search(_load_documentation_browser(image))


def test_dictionary_search_slice_is_exact_raw_linked_and_load_time_pure() -> None:
    runtime = _load_documentation_browser()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)

    runtime = _evaluate_dictionary_search(runtime)

    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert len(SOURCE_LEDGER) == 14
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address
    for name in ZERO_VARIABLES:
        variable = runtime.find(name)
        assert variable is not None
        assert runtime.memory.read64(variable.body_address) == 0

    prior_header = latest_before
    for word in published:
        assert runtime.memory.read64(word.header_address) == prior_header
        assert runtime.memory.read8(word.header_address + 8) == len(word.name)
        assert runtime.memory.read_bytes(
            word.header_address + 9,
            len(word.name),
        ) == word.name
        assert _execute(runtime, "ENTRY>LINK", word.header_address) == (
            prior_header,
        )
        assert _execute(runtime, "ENTRY>NAME", word.header_address) == (
            word.header_address + 9,
            len(word.name),
        )
        prior_header = word.header_address

    immediate = runtime.find("[")
    assert immediate is not None
    flags_length = runtime.memory.read8(immediate.header_address + 8)
    assert flags_length & 0x80
    assert flags_length & 0x7F == len(immediate.name)
    assert _execute(runtime, "ENTRY>NAME", immediate.header_address) == (
        immediate.header_address + 9,
        len(immediate.name),
    )

    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"
    assert runtime.numeric_base == 10
    assert runtime.spinlocks.owner(2) is None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_icontains_covers_nested_loop_paths_without_stack_leaks() -> None:
    cases = (
        (b"", b"", TRUE),
        (b"longer", b"short", 0),
        (b"same", b"same", TRUE),
        (b"pre", b"prefix", TRUE),
        (b"efi", b"prefix", TRUE),
        (b"fix", b"prefix", TRUE),
        (b"preX", b"prefix", 0),
        (b"absent", b"prefix", 0),
        (b"mIx", b"xxMIXyy", TRUE),
        (b"\xe0", b"\xe0", TRUE),
        (b"\xe0", b"\xc0", 0),
    )
    runtime = _load_dictionary_search()
    pattern_buffer = runtime.define_created(
        "IC-PATTERN-CASES",
        initial_body=bytes(64),
    )
    subject_buffer = runtime.define_created(
        "IC-SUBJECT-CASES",
        initial_body=bytes(64),
    )
    context = runtime.new_context()
    return_sentinel = 0xA5A5_5A5A
    context.returns.push(return_sentinel)
    for pattern, subject, expected in cases:
        runtime.memory.write_bytes(pattern_buffer.body_address, pattern)
        runtime.memory.write_bytes(subject_buffer.body_address, subject)
        for value in (
            pattern_buffer.body_address,
            len(pattern),
            subject_buffer.body_address,
            len(subject),
        ):
            context.data.push(value)

        runtime.execute("ICONTAINS?", context=context, step_budget=25_000)

        assert context.data.pop() == expected
        assert context.data.snapshot() == ()
        assert context.returns.snapshot() == (return_sentinel,)
        assert _variable(runtime, "IC-PA") == pattern_buffer.body_address
        assert _variable(runtime, "IC-PL") == len(pattern)
        assert _variable(runtime, "IC-SA") == subject_buffer.body_address
        assert _variable(runtime, "IC-SL") == len(subject)


def test_words_like_is_newest_first_and_keeps_word_scratch_transient() -> None:
    runtime = _load_dictionary_search()
    runtime.evaluate(
        b": ALPHA-NEEDLEQX ;\n"
        b": needleqx ;\n"
        b": OTHER ;\n"
        b": alpha-needleqx ;\n",
        source_name="dictionary-search-shadow-cases",
    )
    assert sum(
        word.name.upper() == b"ALPHA-NEEDLEQX"
        for word in runtime.dictionary.words
    ) == 2
    here = runtime.dictionary.here
    expected = (
        b"alpha-needleqx needleqx ALPHA-NEEDLEQX \r\n"
        b" (3  found)\r\n"
    )

    runtime.evaluate(
        b"WORDS-LIKE NeEdLeQx",
        source_name="words-like-shadowed-duplicates",
        step_budget=SEARCH_STEP_BUDGET,
    )

    assert runtime.drain_uart_output() == expected
    assert _variable(runtime, "WL-CNT") == 3
    assert _variable(runtime, "WL-ENT") == 0
    assert _variable(runtime, "WL-PA") == here + 1
    assert _variable(runtime, "WL-PL") == len(b"NeEdLeQx")
    assert runtime.memory.read_bytes(here, len(b"NeEdLeQx") + 2) == (
        bytes((len(b"NeEdLeQx"),)) + b"NeEdLeQx\0"
    )
    assert runtime.dictionary.here == here

    runtime.evaluate(
        b"APROPOS NeEdLeQx",
        source_name="apropos-alias-equivalence",
        step_budget=SEARCH_STEP_BUDGET,
    )
    assert runtime.drain_uart_output() == expected
    assert runtime.dictionary.here == here

    missing = b"NO-SUCH-QZX"
    runtime.evaluate(
        b"WORDS-LIKE " + missing,
        source_name="words-like-no-match",
        step_budget=SEARCH_STEP_BUDGET,
    )
    assert runtime.drain_uart_output() == b"\r\n (0  found)\r\n"
    assert _variable(runtime, "WL-CNT") == 0
    assert _variable(runtime, "WL-ENT") == 0
    assert _variable(runtime, "WL-PA") == here + 1
    assert _variable(runtime, "WL-PL") == len(missing)
    state_before_usage = tuple(
        _variable(runtime, name)
        for name in ("WL-CNT", "WL-ENT", "WL-PA", "WL-PL")
    )

    runtime.evaluate(b"WORDS-LIKE", source_name="words-like-usage")
    assert runtime.drain_uart_output() == (
        b" Usage: WORDS-LIKE <pattern>\r\n"
    )
    assert tuple(
        _variable(runtime, name)
        for name in ("WL-CNT", "WL-ENT", "WL-PA", "WL-PL")
    ) == state_before_usage
    assert runtime.memory.read_bytes(here, 2) == b"\0\0"
    assert runtime.dictionary.here == here
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_recent_handles_zero_small_and_counts_beyond_the_chain() -> None:
    runtime = _load_dictionary_search()
    words = runtime.dictionary.words
    here = runtime.dictionary.here
    assert tuple(word.name for word in words[-3:]) == (
        b"WORDS-LIKE",
        b"APROPOS",
        b".RECENT",
    )

    assert _execute(runtime, ".RECENT", 0) == ()
    assert runtime.drain_uart_output() == b"\r\n Recent words:\r\n\r\n"

    assert _execute(runtime, ".RECENT", 3) == ()
    assert runtime.drain_uart_output() == (
        b"\r\n Recent words:\r\n"
        b".RECENT APROPOS WORDS-LIKE \r\n"
    )

    assert _execute(runtime, ".RECENT", len(words) + 17) == ()
    expected_names = b"".join(
        word.name + b" " for word in reversed(words)
    )
    assert runtime.drain_uart_output() == (
        b"\r\n Recent words:\r\n" + expected_names + b"\r\n"
    )
    assert runtime.dictionary.words == words
    assert runtime.dictionary.here == here
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_recent_obeys_finite_raw_header_corruption() -> None:
    runtime = _load_dictionary_search()
    recent = runtime.find(".RECENT")
    assert recent is not None
    original_link = runtime.memory.read64(recent.header_address)
    original_flags_length = runtime.memory.read8(recent.header_address + 8)

    # Keep the corrupt chain deliberately finite: a raw zero link stops after
    # the newest header, while a raw length of four clips its displayed name.
    runtime.memory.write64(recent.header_address, 0)
    runtime.memory.write8(recent.header_address + 8, 4)
    assert _execute(runtime, "ENTRY>LINK", recent.header_address) == (0,)
    assert _execute(runtime, "ENTRY>NAME", recent.header_address) == (
        recent.header_address + 9,
        4,
    )

    assert _execute(runtime, ".RECENT", 99) == ()
    assert runtime.drain_uart_output() == (
        b"\r\n Recent words:\r\n.REC \r\n"
    )

    runtime.memory.write64(recent.header_address, original_link)
    runtime.memory.write8(
        recent.header_address + 8,
        original_flags_length,
    )
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
