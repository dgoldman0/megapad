"""Contiguous unchanged-source acceptance for KDOS MARKER and FORGET."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE_DIRECTORY = Path(__file__).with_name("fixtures")
BASE_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-39-69.f"
PARSE_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-71-115.f"
ALLOCATOR_FIXTURE = FIXTURE_DIRECTORY / "kdos-allocator-116-545.f"
SNAPSHOT_FIXTURE = FIXTURE_DIRECTORY / "kdos-snapshots-546-617.f"

MEGAPAD_REVISION = "9576065668114ffdf9b08c015cf4d16c8b2e6e89"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"

SLICES = (
    (
        BASE_FIXTURE,
        39,
        69,
        "e3918ffeab18446da9e9b190b4d0b82382a3ed5e9fcc220680b5164ab261d01c",
        "ecef2fef19b54559367f1a162a97558776ab6ee8",
    ),
    (
        PARSE_FIXTURE,
        71,
        115,
        "a59c8811eef09b2a1bd31b5c0801b68a29cf1434c67bdc17a63d15e60d69a99c",
        "fbfea6100b2dff8925dde073a7bd35a3f88544dc",
    ),
    (
        ALLOCATOR_FIXTURE,
        116,
        545,
        "0a7d819a0a17ab96378771f69e6ca3dbf2bc2570028977a713bcba0742e22106",
        "46dcb6e2c82d57904f7d92d43292bf3670ba5347",
    ),
    (
        SNAPSHOT_FIXTURE,
        546,
        617,
        "9380a7828dfaae383501cee5566f058b783c85ce450763e091d52e7d19c17d56",
        "3a78ac1da4d8df75dfa0d31bd3b49dee029592ea",
    ),
)

SNAPSHOT_DEFINITIONS = (
    b"MARKER",
    b"(ENTRY>NAME)",
    b"FG-A",
    b"FG-L",
    b"FORGET",
)


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _verified_slice(
    fixture: Path,
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


def _load_snapshots() -> MegaForthRuntime:
    runtime = MegaForthRuntime()
    results = []
    for fixture, first, last, sha256, git_blob in SLICES:
        source = _verified_slice(fixture, first, last, sha256, git_blob)
        results.append(
            runtime.evaluate(
                source,
                source_name=f"kdos.f@{MEGAPAD_REVISION}:{first}-{last}",
            )
        )

    assert tuple(word.name for word in results[-1].definitions) == (
        SNAPSHOT_DEFINITIONS
    )
    for name in ("FG-A", "FG-L"):
        variable = runtime.find(name)
        assert variable is not None
        assert runtime.memory.read64(variable.body_address) == 0
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


@pytest.fixture
def loaded_snapshots() -> MegaForthRuntime:
    return _load_snapshots()


def test_entry_name_reads_the_exact_live_header_layout(
    loaded_snapshots: MegaForthRuntime,
) -> None:
    runtime = loaded_snapshots
    target = runtime.find("IF")
    assert target is not None
    assert runtime.memory.read8(target.header_address + 8) & 0x80
    runtime.main_context.data.push(target.header_address)

    runtime.execute("(ENTRY>NAME)")

    assert runtime.main_context.data.snapshot() == (
        target.header_address + 9,
        len(target.name),
    )
    assert runtime.memory.read_bytes(target.header_address + 9, len(target.name)) == (
        target.name
    )


def test_marker_restores_here_latest_shadowing_and_stale_bytes(
    loaded_snapshots: MegaForthRuntime,
) -> None:
    runtime = loaded_snapshots
    original = runtime.evaluate(b": PANEL 1 ;").definitions[0]
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest

    runtime.evaluate(b"MARKER CLEAN")
    marker = runtime.find("CLEAN")
    assert marker is not None
    assert marker.header_address == saved_here
    assert runtime.memory.read64(marker.body_address) == saved_latest
    assert runtime.memory.read64(marker.body_address + 8) == saved_here

    replacement = runtime.evaluate(b": panel 2 ;").definitions[0]
    later = runtime.evaluate(b": LATER 3 ;").definitions[0]
    active_here = runtime.dictionary.here
    stale_bytes = runtime.memory.read_bytes(saved_here, active_here - saved_here)

    runtime.execute("CLEAN")

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.dictionary.here == saved_here
    assert runtime.dictionary.latest == saved_latest
    assert runtime.find("CLEAN") is None
    assert runtime.find("PANEL") is original
    assert runtime.find("LATER") is None
    for removed in (marker, replacement, later):
        with pytest.raises(KeyError):
            runtime.dictionary.resolve(removed.xt)
    assert runtime.memory.read_bytes(saved_here, len(stale_bytes)) == stale_bytes

    reused = runtime.evaluate(b": REUSED 4 ;").definitions[0]
    assert reused.header_address == saved_here


def test_forget_case_insensitively_removes_one_shadow_and_newer_words(
    loaded_snapshots: MegaForthRuntime,
) -> None:
    runtime = loaded_snapshots
    original = runtime.evaluate(b": PANEL 1 ;").definitions[0]
    replacement = runtime.evaluate(b": panel 2 ;").definitions[0]
    same_length_miss = runtime.evaluate(b": OTHER 3 ;").definitions[0]
    assert len(same_length_miss.name) == len(replacement.name)
    active_here = runtime.dictionary.here
    stale_bytes = runtime.memory.read_bytes(
        replacement.header_address,
        active_here - replacement.header_address,
    )

    runtime.evaluate(b"FORGET PaNeL")

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.dictionary.here == replacement.header_address
    assert runtime.dictionary.latest_word is original
    assert runtime.find("PANEL") is original
    assert runtime.find("OTHER") is None
    for removed in (replacement, same_length_miss):
        with pytest.raises(KeyError):
            runtime.dictionary.resolve(removed.xt)
    assert runtime.memory.read_bytes(
        replacement.header_address,
        len(stale_bytes),
    ) == stale_bytes

    reused = runtime.evaluate(b": NEXT 4 ;").definitions[0]
    assert reused.header_address == replacement.header_address


@pytest.mark.parametrize(
    ("source", "diagnostic"),
    (
        (b"FORGET", b"Usage: FORGET <name>"),
        (b"FORGET NO-SUCH-WORD", b"FORGET: not found"),
    ),
)
def test_forget_diagnostics_abort_without_changing_the_dictionary(
    source: bytes,
    diagnostic: bytes,
) -> None:
    runtime = _load_snapshots()
    active_here = runtime.dictionary.here
    active_latest = runtime.dictionary.latest
    live_forget = runtime.find("FORGET")
    assert live_forget is not None

    with pytest.raises(ForthAbort):
        runtime.evaluate(source)

    assert runtime.drain_uart_output() == diagnostic
    assert runtime.dictionary.here == active_here
    assert runtime.dictionary.latest == active_latest
    assert runtime.find("FORGET") is live_forget
    assert runtime.dictionary.resolve(live_forget.xt) is live_forget
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
