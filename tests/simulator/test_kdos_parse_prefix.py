"""Unchanged-source acceptance for KDOS parsing and stack utilities."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from simulator.errors import ExecutionError, ForthAbort
from simulator.ir import AbortIf, WriteOutput
from simulator.runtime import ColonDefinition, MegaForthRuntime


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE_DIRECTORY = Path(__file__).with_name("fixtures")
BASE_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-39-69.f"
PARSE_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-71-115.f"

MEGAPAD_REVISION = "ed451faccfddb5f3fbb4e2200eb0dd0fdc314f4c"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"

BASE_FIRST_LINE = 39
BASE_LAST_LINE = 69
BASE_SHA256 = "e3918ffeab18446da9e9b190b4d0b82382a3ed5e9fcc220680b5164ab261d01c"
BASE_GIT_BLOB = "ecef2fef19b54559367f1a162a97558776ab6ee8"

PARSE_FIRST_LINE = 71
PARSE_LAST_LINE = 115
PARSE_SHA256 = "a59c8811eef09b2a1bd31b5c0801b68a29cf1434c67bdc17a63d15e60d69a99c"
PARSE_GIT_BLOB = "fbfea6100b2dff8925dde073a7bd35a3f88544dc"
PARSE_DEFINITIONS = (
    b"NAMEBUF",
    b"PATHBUF",
    b"PN-LEN",
    b"PARSE-NAME",
    b"NEEDS",
    b"ASSERT",
    b".DEPTH",
    b"0>=",
)


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _verified_slice(
    fixture: Path,
    *,
    first_line: int,
    last_line: int,
    sha256: str,
    git_blob: str,
) -> bytes:
    source = fixture.read_bytes()
    assert hashlib.sha256(source).hexdigest() == sha256
    assert _git_blob_id(source) == git_blob

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[first_line - 1 : last_line])
    return source


@pytest.fixture
def loaded_parse_prefix() -> MegaForthRuntime:
    base = _verified_slice(
        BASE_FIXTURE,
        first_line=BASE_FIRST_LINE,
        last_line=BASE_LAST_LINE,
        sha256=BASE_SHA256,
        git_blob=BASE_GIT_BLOB,
    )
    parse = _verified_slice(
        PARSE_FIXTURE,
        first_line=PARSE_FIRST_LINE,
        last_line=PARSE_LAST_LINE,
        sha256=PARSE_SHA256,
        git_blob=PARSE_GIT_BLOB,
    )

    runtime = MegaForthRuntime()
    runtime.evaluate(
        base,
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:{BASE_FIRST_LINE}-{BASE_LAST_LINE}"
        ),
    )
    result = runtime.evaluate(
        parse,
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:"
            f"{PARSE_FIRST_LINE}-{PARSE_LAST_LINE}"
        ),
    )

    assert tuple(word.name for word in result.definitions) == PARSE_DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _body(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def test_variable_allot_layout_matches_the_unchanged_prefix(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    namebuf = runtime.find("NAMEBUF")
    pathbuf = runtime.find("PATHBUF")
    pn_len = runtime.find("PN-LEN")
    parse_name = runtime.find("PARSE-NAME")
    assert namebuf is not None
    assert pathbuf is not None
    assert pn_len is not None
    assert parse_name is not None

    assert pathbuf.header_address == namebuf.body_address + 8 + 23
    assert pn_len.header_address == pathbuf.body_address + 8 + 127
    assert parse_name.header_address == pn_len.body_address + 8


def test_parse_name_retains_full_path_and_clamps_component(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    path = b"lib/crypto/implementations/aes-256.f"

    runtime.evaluate(b"PARSE-NAME      " + path)

    name_address = _body(runtime, "NAMEBUF")
    path_address = _body(runtime, "PATHBUF")
    pn_length_address = _body(runtime, "PN-LEN")
    assert runtime.memory.read64(pn_length_address) == 23
    assert runtime.memory.read_bytes(name_address, 24) == path[:23] + b"\0"
    assert runtime.memory.read_bytes(path_address, 128) == (
        path + bytes(128 - len(path))
    )


def test_parse_name_clamps_127_bytes_and_clears_previous_tails(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    long_name = b"x" * 140
    runtime.evaluate(b"PARSE-NAME " + long_name)

    name_address = _body(runtime, "NAMEBUF")
    path_address = _body(runtime, "PATHBUF")
    assert runtime.memory.read_bytes(name_address, 24) == b"x" * 23 + b"\0"
    assert runtime.memory.read_bytes(path_address, 128) == b"x" * 127 + b"\0"

    runtime.evaluate(b"PARSE-NAME z")
    assert runtime.memory.read64(_body(runtime, "PN-LEN")) == 1
    assert runtime.memory.read_bytes(name_address, 24) == b"z" + bytes(23)
    assert runtime.memory.read_bytes(path_address, 128) == b"z" + bytes(127)


def test_word_publishes_a_transient_counted_string_at_unchanged_here(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    here = runtime.dictionary.here

    runtime.evaluate(b"BL WORD    alpha")

    assert runtime.main_context.data.snapshot() == (here,)
    assert runtime.dictionary.here == here
    assert runtime.memory.read_bytes(here, 7) == b"\x05alpha\0"

    runtime.main_context.data.clear()
    runtime.evaluate(b"BL WORD alpha\tbeta")
    assert runtime.memory.read_bytes(here, 12) == b"\x0aalpha\tbeta\0"

    runtime.main_context.data.clear()
    runtime.evaluate(b"BL WORD")
    assert runtime.memory.read_bytes(here, 2) == b"\0\0"

    outside_input = runtime.new_context()
    outside_input.data.push(32)
    with pytest.raises(ExecutionError, match="active input line"):
        runtime.execute("WORD", context=outside_input)


def test_word_capacity_failure_does_not_commit_the_input_cursor(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    observed_after_failure: list[bytes] = []

    def capture_uncommitted_cursor(_context) -> None:
        observed_after_failure.append(runtime.parse_input_word())
        raise ExecutionError("guarded WORD capacity")

    hook = runtime.define_primitive(
        "HOST-CAPTURE-WORD-FAULT",
        capture_uncommitted_cursor,
    )
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    bank0 = runtime.memory.regions[0]
    runtime.dictionary.allot(bank0.limit - runtime.dictionary.here - 1)
    context = runtime.new_context()

    with pytest.raises(ExecutionError, match="guarded WORD capacity"):
        runtime.evaluate(b"32 WORD alpha", context=context)

    assert observed_after_failure == [b"alpha"]
    assert runtime.dictionary.here == bank0.limit - 1
    assert context.data.snapshot() == ()


def test_abort_quote_operations_keep_exact_payloads_and_stack_behavior(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    needs = runtime.find("NEEDS")
    assertion = runtime.find("ASSERT")
    assert needs is not None and isinstance(needs.implementation, ColonDefinition)
    assert assertion is not None and isinstance(
        assertion.implementation,
        ColonDefinition,
    )
    assert [
        operation.payload
        for operation in needs.implementation.operations
        if isinstance(operation, AbortIf)
    ] == [b"Stack underflow"]
    assert [
        operation.payload
        for operation in assertion.implementation.operations
        if isinstance(operation, AbortIf)
    ] == [b"Assertion failed"]

    enough = runtime.new_context()
    for value in (11, 22, 2):
        enough.data.push(value)
    runtime.execute("NEEDS", context=enough)
    assert enough.data.snapshot() == (11, 22)

    insufficient = runtime.new_context()
    insufficient.data.push(11)
    insufficient.data.push(2)
    with pytest.raises(ForthAbort):
        runtime.execute("NEEDS", context=insufficient)
    assert insufficient.data.snapshot() == ()
    assert insufficient.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Stack underflow"

    truth = runtime.new_context()
    truth.data.push(TRUE)
    runtime.execute("ASSERT", context=truth)
    assert truth.data.snapshot() == ()

    falsehood = runtime.new_context()
    falsehood.data.push(0)
    with pytest.raises(ForthAbort):
        runtime.execute("ASSERT", context=falsehood)
    assert runtime.drain_uart_output() == b"Assertion failed"


def test_dot_quote_compiles_exact_literals_and_prints_in_interpret_state(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    dot_depth = runtime.find(".DEPTH")
    assert dot_depth is not None
    assert isinstance(dot_depth.implementation, ColonDefinition)
    assert [
        operation.payload
        for operation in dot_depth.implementation.operations
        if isinstance(operation, WriteOutput)
    ] == [b" [", b" deep]"]

    context = runtime.new_context()
    context.data.push(10)
    context.data.push(20)
    runtime.execute(".DEPTH", context=context)
    assert context.data.snapshot() == (10, 20)
    assert runtime.drain_uart_output() == b" [2  deep]"

    runtime.evaluate(b'." immediate" 9')
    assert runtime.main_context.data.snapshot() == (9,)
    assert runtime.drain_uart_output() == b"immediate"


@pytest.mark.parametrize(
    ("value", "expected"),
    (
        (0, TRUE),
        (1, TRUE),
        ((1 << 63) - 1, TRUE),
        (1 << 63, 0),
        (MASK64, 0),
    ),
)
def test_zero_greater_equal_uses_signed_test_and_full_width_invert(
    loaded_parse_prefix: MegaForthRuntime,
    value: int,
    expected: int,
) -> None:
    context = loaded_parse_prefix.new_context()
    context.data.push(value)
    loaded_parse_prefix.execute("0>=", context=context)
    assert context.data.snapshot() == (expected,)


def test_bios_min_max_and_greater_than_share_signed_ordering(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    context = runtime.new_context()
    context.data.push(MASK64)
    context.data.push(1)
    runtime.execute("MIN", context=context)
    assert context.data.snapshot() == (MASK64,)

    context.data.clear()
    context.data.push(MASK64)
    context.data.push(1)
    runtime.execute("MAX", context=context)
    assert context.data.snapshot() == (1,)

    context.data.clear()
    context.data.push(MASK64)
    context.data.push(0)
    runtime.execute(">", context=context)
    assert context.data.snapshot() == (0,)


def test_new_scalar_primitives_wrap_and_report_pre_push_depth(
    loaded_parse_prefix: MegaForthRuntime,
) -> None:
    runtime = loaded_parse_prefix
    context = runtime.new_context()

    context.data.push(0)
    context.data.push(1)
    runtime.execute("-", context=context)
    assert context.data.snapshot() == (MASK64,)

    runtime.execute("1+", context=context)
    assert context.data.snapshot() == (0,)
    runtime.execute("INVERT", context=context)
    assert context.data.snapshot() == (MASK64,)

    context.data.push(7)
    runtime.execute("DEPTH", context=context)
    assert context.data.snapshot() == (MASK64, 7, 2)

    runtime.memory.write8(0x80_123, 0xE7)
    byte_context = runtime.new_context()
    byte_context.data.push(0x80_123)
    runtime.execute("C@", context=byte_context)
    assert byte_context.data.snapshot() == (0xE7,)
