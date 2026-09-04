"""Unchanged-source acceptance for the first executable KDOS prefix."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import FALSE, MASK64, TRUE
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
PREFIX_FIXTURE = Path(__file__).with_name("fixtures") / "kdos-prefix-39-69.f"

# The fixture is logical lines 39 through 69, inclusive, from this immutable
# MegaPad revision and kdos.f Git blob.  Pinning both the fixture digest and its
# blob identity makes an accidental edit distinguishable from an intentional
# refresh after the ordinary KDOS source changes.
MEGAPAD_REVISION = "9576065668114ffdf9b08c015cf4d16c8b2e6e89"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"
PREFIX_FIRST_LINE = 39
PREFIX_LAST_LINE = 69
PREFIX_SHA256 = "e3918ffeab18446da9e9b190b4d0b82382a3ed5e9fcc220680b5164ab261d01c"
PREFIX_GIT_BLOB = "ecef2fef19b54559367f1a162a97558776ab6ee8"
PREFIX_DEFINITIONS = (b".R", b"DEFER", b"IS", b"SAMESTR?")


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _read_verified_prefix() -> bytes:
    source = PREFIX_FIXTURE.read_bytes()
    assert hashlib.sha256(source).hexdigest() == PREFIX_SHA256
    assert _git_blob_id(source) == PREFIX_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    kdos_lines = complete_kdos.splitlines(keepends=True)
    current_slice = b"".join(
        kdos_lines[PREFIX_FIRST_LINE - 1 : PREFIX_LAST_LINE]
    )
    assert source == current_slice
    return source


@pytest.fixture
def loaded_prefix() -> MegaForthRuntime:
    source = _read_verified_prefix()
    runtime = MegaForthRuntime()

    # A hosted backend may implement these optimization controls as semantic
    # no-ops, but the public BIOS word must still be executable.  The unchanged
    # prefix invokes it again as its sole interpret-state executable token.
    jit_result = runtime.execute("JIT-ON")
    assert jit_result.semantic_steps == 1
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    result = runtime.evaluate(
        source,
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:"
            f"{PREFIX_FIRST_LINE}-{PREFIX_LAST_LINE}"
        ),
    )

    assert tuple(word.name for word in result.definitions) == PREFIX_DEFINITIONS
    assert result.semantic_steps >= 1
    return runtime


@pytest.mark.parametrize(
    ("left", "right", "length", "expected"),
    (
        (b"calendar\0", b"calendar\0", 9, TRUE),
        (b"calendar\0", b"calender\0", 9, FALSE),
        (b"pad\0xxxx", b"pad\0yyyy", 4, TRUE),
    ),
)
def test_samestr_compares_caller_owned_guest_bytes(
    loaded_prefix: MegaForthRuntime,
    left: bytes,
    right: bytes,
    length: int,
    expected: int,
) -> None:
    left_address = 0x80_000
    right_address = 0x80_100
    loaded_prefix.memory.write_bytes(left_address, left)
    loaded_prefix.memory.write_bytes(right_address, right)

    context = loaded_prefix.new_context()
    context.data.push(left_address)
    context.data.push(right_address)
    context.data.push(length)
    loaded_prefix.execute("SAMESTR?", context=context)

    assert context.data.snapshot() == (expected,)
    assert context.returns.snapshot() == ()


def test_defer_defaults_to_abort_through_its_guest_body_cell(
    loaded_prefix: MegaForthRuntime,
) -> None:
    definition = loaded_prefix.evaluate(b"DEFER ACTION", source_name="defer.f")
    assert tuple(word.name for word in definition.definitions) == (b"ACTION",)

    abort = loaded_prefix.find("ABORT")
    assert abort is not None
    inspection = loaded_prefix.new_context()
    loaded_prefix.evaluate(b"' ACTION >BODY DUP @", context=inspection)
    body_address, default_xt = inspection.data.snapshot()
    assert default_xt == abort.xt
    assert loaded_prefix.memory.read64(body_address) == abort.xt

    aborted = loaded_prefix.new_context()
    aborted.data.push(123)
    aborted.returns.push(456)
    with pytest.raises(ForthAbort):
        loaded_prefix.execute("ACTION", context=aborted)
    assert aborted.data.snapshot() == ()
    assert aborted.returns.snapshot() == ()


def test_is_rebinds_deferred_source_words_via_guest_memory(
    loaded_prefix: MegaForthRuntime,
) -> None:
    loaded_prefix.evaluate(
        b"DEFER ACTION : CALL-ACTION ACTION ; "
        b": FIRST-ACTION 11 ; : SECOND-ACTION 22 ;",
        source_name="deferred-actions.f",
    )
    action = loaded_prefix.find("ACTION")
    assert action is not None

    inspection = loaded_prefix.new_context()
    loaded_prefix.evaluate(b"' ACTION >BODY", context=inspection)
    body_address, = inspection.data.snapshot()

    for target, expected in ((b"FIRST-ACTION", 11), (b"SECOND-ACTION", 22)):
        target_word = loaded_prefix.find(target)
        assert target_word is not None

        loaded_prefix.evaluate(b"' " + target + b" IS ACTION")
        assert loaded_prefix.memory.read64(body_address) == target_word.xt
        assert loaded_prefix.find("ACTION") is action

        context = loaded_prefix.new_context()
        loaded_prefix.execute("CALL-ACTION", context=context)
        assert context.data.snapshot() == (expected,)
        assert context.returns.snapshot() == ()


def test_prefix_definitions_keep_the_bios_xt_bindings_captured_at_compile_time(
    loaded_prefix: MegaForthRuntime,
) -> None:
    loaded_prefix.evaluate(b": ORIGINAL-TARGET 77 ;")
    target = loaded_prefix.find("ORIGINAL-TARGET")
    assert target is not None

    def reject_shadow(name: bytes):
        def rejected(_context):
            raise AssertionError(f"shadowed BIOS word {name!r} executed")

        return rejected

    captured_names = (
        b"DROP",
        b".",
        b"CREATE",
        b",",
        b"ABORT",
        b"@",
        b"EXECUTE",
        b"'",
        b">BODY",
        b"!",
    )
    original_xts = {
        name: loaded_prefix.find(name).xt  # type: ignore[union-attr]
        for name in captured_names
    }
    for name in captured_names:
        shadow = loaded_prefix.define_primitive(name, reject_shadow(name))
        assert shadow.xt != original_xts[name]

    numeric = loaded_prefix.new_context()
    numeric.data.push(-9)
    numeric.data.push(20)
    loaded_prefix.execute(".R", context=numeric)
    assert loaded_prefix.drain_uart_output() == b"-9 "

    loaded_prefix.evaluate(b"DEFER STABLE-ACTION")
    with pytest.raises(ForthAbort):
        loaded_prefix.execute("STABLE-ACTION", context=loaded_prefix.new_context())

    rebinding = loaded_prefix.new_context()
    rebinding.data.push(target.xt)
    loaded_prefix.evaluate(b"IS STABLE-ACTION", context=rebinding)
    assert rebinding.data.snapshot() == ()

    invoked = loaded_prefix.new_context()
    loaded_prefix.execute("STABLE-ACTION", context=invoked)
    assert invoked.data.snapshot() == (77,)
    assert invoked.returns.snapshot() == ()


@pytest.mark.parametrize(
    ("base", "value", "expected"),
    (
        (10, -42, b"-42 "),
        (10, 0, b"0 "),
        (10, -(1 << 63), b"-9223372036854775808 "),
        (16, -42, b"-2A "),
    ),
)
def test_dot_r_uses_the_stable_bios_dot_binding(
    loaded_prefix: MegaForthRuntime,
    base: int,
    value: int,
    expected: bytes,
) -> None:
    loaded_prefix.set_numeric_base(base)
    context = loaded_prefix.new_context()
    context.data.push(value)
    context.data.push(40)

    loaded_prefix.execute(".R", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert loaded_prefix.drain_uart_output() == expected


@pytest.mark.parametrize(
    ("left", "right", "left_length", "right_length", "expected"),
    (
        (b"abc", b"abc", 3, 3, 0),
        (b"abc", b"abd", 3, 3, MASK64),
        (b"abe", b"abd", 3, 3, 1),
        (b"abc", b"abcd", 3, 4, MASK64),
        (b"abcd", b"abc", 4, 3, 1),
        (b"\x80", b"\x7f", 1, 1, 1),
        (b"", b"", 0, 0, 0),
    ),
)
def test_bios_compare_binding_used_by_samestr_is_exact(
    loaded_prefix: MegaForthRuntime,
    left: bytes,
    right: bytes,
    left_length: int,
    right_length: int,
    expected: int,
) -> None:
    left_address = 0x81_003
    right_address = 0x81_105
    loaded_prefix.memory.write_bytes(left_address, left)
    loaded_prefix.memory.write_bytes(right_address, right)
    context = loaded_prefix.new_context()
    for value in (left_address, left_length, right_address, right_length):
        context.data.push(value)

    loaded_prefix.execute("COMPARE", context=context)

    assert context.data.snapshot() == (expected,)
    assert context.returns.snapshot() == ()
