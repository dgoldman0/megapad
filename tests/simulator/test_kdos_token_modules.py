"""KDOS loads modules from token files, in the hosted simulator.

``tests/token_module_cases.py`` holds the cases: what each volume holds,
what is typed, and what the output must show.  ``tests/test_system.py``
(``TestKDOSTokenModules``) runs the same ones on the BIOS.  The tests of
the recorder and of the file's layout are here alone, since both belong
to the tools that build images.
"""

from __future__ import annotations

import struct
from pathlib import Path

import pytest

from shared.boot_identity import boot_identity, text_identity
from shared.token_form import (
    FLAG_SOURCE_BESIDE,
    HEADER_BYTES,
    TokenFormError,
    is_token_file,
    pack_module_name_file,
    pack_token_file,
    unpack_token_file,
    with_stamp,
)
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.token_module_cases import (
    A,
    CASES,
    RECORDED_LINES,
    RECORDED_SHOWS,
    RECORDED_SOURCES,
    TOKENS_ON,
    Case,
    failures,
    record,
    recorded_volume,
    records,
    stamps,
    tokens,
    volume,
)

KDOS = (Path(__file__).resolve().parents[2] / "kdos.f").read_bytes()


def _run(files: dict[str, bytes], lines: list[str]) -> str:
    runtime = MegaForthRuntime(
        storage=HostedStorageService(bytearray(volume(files).img))
    )
    runtime.evaluate(KDOS, source_name="kdos.f")
    mark = len(runtime.uart_output)
    for line in lines:
        runtime.evaluate(line.encode(), source_name="case")
    assert runtime.main_context.returns.snapshot() == ()
    return bytes(runtime.uart_output[mark:]).decode("latin-1")


@pytest.mark.parametrize("name", sorted(CASES))
def test_a_case_runs_as_it_does_on_the_bios(name: str) -> None:
    case = CASES[name]

    assert failures(case, _run(case.files, case.lines)) == []


# ---- the recorder ------------------------------------------------------------


def test_recorded_files_load_as_their_source_does() -> None:
    shows = Case(files={}, lines=[], must=RECORDED_SHOWS)
    from_source = _run(RECORDED_SOURCES, RECORDED_LINES[:-1])
    assert failures(shows, from_source) == []

    files = recorded_volume(RECORDED_SOURCES, "top.f")
    assert sorted(files) == ["lib.ft", "top.ft"]
    from_tokens = _run(files, [TOKENS_ON, *RECORDED_LINES])
    after = stamps(RECORDED_SOURCES["top.f"], RECORDED_SOURCES["lib.f"])[2]
    shows.must.append(rf"\[STAMP {after >> 32} {after & 0xFFFFFFFF} \]")
    assert failures(shows, from_tokens) == []
    # The two loads print the same text, apart from the stamp's line.
    assert " ".join(from_tokens.split()).startswith(" ".join(from_source.split()))


def test_the_recorder_notes_the_order_of_the_loads_and_what_was_loading() -> None:
    recorder, _ = record(RECORDED_SOURCES, "top.f")

    top, lib = recorder.units
    assert (top.source, lib.source) == (
        RECORDED_SOURCES["top.f"], RECORDED_SOURCES["lib.f"],
    )
    assert (top.index, top.enclosing) == (0, ())
    assert (lib.index, lib.enclosing) == (1, (0,))
    assert top.module_name() == b"rec-top" and lib.module_name() == b"rec-lib"
    assert top.text_identity == text_identity(RECORDED_SOURCES["top.f"])
    # A stamp per file, as the files' texts in that order give it; a file
    # whose text the builder writes again is taken by the text it will have.
    boot = boot_identity()
    assert recorder.stamps(boot) == stamps(top.source, lib.source)[:2]
    assert recorder.stamps(boot, {0: b"other\n"}) == stamps(b"other\n", lib.source)[:2]
    assert recorder.stamps(boot ^ 1)[0] != recorder.stamps(boot)[0]


def test_a_text_no_loader_announced_is_no_file_of_the_load() -> None:
    held = b": HELD-WORD 3 ;\n"
    cells = " ".join(f"{byte} C," for byte in held)
    sources = {
        "holder.f": (
            f"CREATE HELD {cells}\n"
            f"HELD {len(held)} SOURCE-EVALUATE-CHECKED DROP\n"
            f"HELD {len(held)} 2DUP LOAD-STAMP-SOURCE SOURCE-EVALUATE-CHECKED DROP\n"
        ).encode(),
    }
    recorder, _ = record(sources, "holder.f")

    holder, quiet, told = recorder.units
    assert (holder.kind, quiet.kind, told.kind) == ("loader", "source", "source")
    assert quiet.source == told.source == held
    assert (holder.announced, quiet.announced, told.announced) == (True, False, True)
    assert told.enclosing == (0,)
    # The stamp where each began: the text that was not announced leaves it
    # as it was, and the one that was is taken in.
    assert recorder.stamps(boot_identity()) == [
        stamps()[0], stamps(holder.source)[1], stamps(holder.source)[1],
    ]


def test_a_file_that_takes_entries_out_of_the_dictionary_has_no_token_form() -> None:
    sources = {
        "undo.f": b": U-ONE 1 ;\nHERE LATEST DICT-ROLLBACK\n: U-TWO 2 ;\n",
        "keep.f": b": K-ONE 1 ;\n",
        "half.f": b": H-ONE 1 ;\nNO-SUCH-WORD\n",
    }
    recorder, _ = record(sources, "undo.f")
    (undo,) = recorder.units
    with pytest.raises(TokenFormError, match="took entries out"):
        undo.token_file()

    recorder, _ = record(sources, "keep.f")
    assert is_token_file(recorder.units[0].token_file())

    recorder, _ = record(sources, "half.f")
    with pytest.raises(TokenFormError, match="did not finish"):
        recorder.units[0].token_file()


# ---- the file ------------------------------------------------------------------


def test_the_file_round_trips_and_kdos_reads_what_was_written() -> None:
    recorded, slots = records((":", " F-WORD"), 5, ";")
    data = pack_token_file(
        recorded, slots, source=A, lines=3, module_name=b"mod-file",
        stamp=0x0123456789ABCDEF, source_beside=True,
    )
    parts = unpack_token_file(data)

    assert (parts.records, parts.slots, parts.lines) == (recorded, slots, 3)
    assert (parts.module_name, parts.stamp, parts.source_beside) == (
        b"mod-file", 0x0123456789ABCDEF, True,
    )
    assert parts.text_identity == text_identity(A)
    assert parts.header_bytes == HEADER_BYTES + 8 and len(data) % 1 == 0
    assert unpack_token_file(with_stamp(data, 7, source_beside=False)).stamp == 7
    assert not unpack_token_file(with_stamp(data, 7, source_beside=False)).source_beside

    lines = ["CREATE FILE"]
    for start in range(0, len(data), 24):
        lines.append(" ".join(f"{byte} C," for byte in data[start : start + 24]))
    lines += [
        'CR ." [FIELDS " FILE TKF.HEADER . FILE TKF.SLOTS . FILE TKF.RECORDS .',
        "FILE TKF.SOURCE . FILE TKF.FLAGS . FILE TKF.STAMP 32 RSHIFT .",
        "FILE TKF.STAMP 4294967295 AND . FILE TKF.TEXT-ID 32 RSHIFT .",
        'FILE TKF.TEXT-ID 4294967295 AND . ." ]"',
        f'CR ." [NAME " FILE TKF.ID TYPE ." ]"',
        f'CR ." [IS " FILE {len(data)} TOKEN-FILE? . FILE {len(data)} _TKF-VALID? .',
        f'FILE {len(data) - 1} _TKF-VALID? . FILE 63 TOKEN-FILE? . ." ]"',
    ]
    text = " ".join(_run({}, lines).split())
    identity = text_identity(A)
    assert (
        f"[FIELDS {HEADER_BYTES + 8} {slots} {len(recorded)} {len(A)} "
        f"{FLAG_SOURCE_BESIDE} {0x01234567} {0x89ABCDEF} "
        f"{identity >> 32} {identity & 0xFFFFFFFF} ]"
    ) in text
    assert "[NAME mod-file]" in text
    assert "[IS -1 -1 0 0 ]" in text


def test_a_file_that_only_names_its_module_holds_no_records() -> None:
    parts = unpack_token_file(pack_module_name_file(b"mod-early"))

    assert (parts.records, parts.slots, parts.module_name) == (b"", 0, b"mod-early")
    assert (parts.stamp, parts.source_beside, parts.source_bytes) == (0, False, 0)
    with pytest.raises(TokenFormError, match="needs the name"):
        pack_module_name_file(b"")


@pytest.mark.parametrize(
    ("what", "change", "message"),
    [
        ("another magic", lambda d: b"MFTOKEN2" + d[8:], "not a token file"),
        ("fewer bytes than a header", lambda d: d[:63], "not a token file"),
        ("a header below 64 bytes", lambda d: d[:8] + struct.pack("<I", 56) + d[12:],
         "bad header size"),
        ("a header of odd size", lambda d: d[:8] + struct.pack("<I", 76) + d[12:],
         "bad header size"),
        ("a name past the header", lambda d: d[:44] + struct.pack("<H", 99) + d[46:],
         "module name outside"),
        ("a byte more", lambda d: d + b"\0", "sizes do not account"),
        ("a byte less", lambda d: d[:-1], "sizes do not account"),
        ("a flag nothing defines", lambda d: d[:46] + struct.pack("<H", 2) + d[48:],
         "reserved fields"),
        ("a reserved cell in use", lambda d: d[:56] + struct.pack("<Q", 1) + d[64:],
         "reserved fields"),
        ("a changed record", lambda d: d[:-2] + bytes([d[-2] ^ 1]) + d[-1:],
         "record CRC"),
    ],
)
def test_reading_a_file_refuses(what: str, change, message: str) -> None:
    data = tokens("a", 11, stamp=stamps()[0])
    unpack_token_file(data)

    with pytest.raises(TokenFormError, match=message):
        unpack_token_file(change(data))


def test_writing_a_file_refuses_what_the_layout_cannot_hold() -> None:
    with pytest.raises(TokenFormError, match="slot count"):
        pack_token_file(b"\xff", 0xF001, source=b"", lines=0)
    with pytest.raises(TokenFormError, match="stamp out of range"):
        pack_token_file(b"\xff", 1, source=b"", lines=0, stamp=1 << 64)
    with pytest.raises(TokenFormError, match="module name too long"):
        pack_token_file(b"\xff", 1, source=b"", lines=0, module_name=b"x" * 65536)
