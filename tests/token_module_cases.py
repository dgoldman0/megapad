"""KDOS loading modules from token files: the cases both machines run.

``tests/test_system.py`` (``TestKDOSTokenModules``) runs each case on the
BIOS in the emulator, and ``tests/simulator/test_kdos_token_modules.py``
runs it in the hosted simulator.  A case is a set of files for the boot
volume, the lines typed at KDOS's prompt, what the output must show, and
what it must not.

KDOS uses a token file when it was built for this system, which its header
says by the system's ``BOOT-ID``, and, where its source ships beside it,
when that source is the one it was recorded from.  Each case that ends
"refused" plants one fault against that rule or against the file's layout
and holds the loader to refusing the file, naming it, and loading nothing
in its place.  The cases under "what the rule does not look at" pin the
other side: a file loaded earlier that has changed, another load order, an
edited script and a word typed first leave a token file in use.

The token files are written by hand here, so that a file can state a
header its records do not match.  A file recorded from a text that defines
``A-WORD`` as 11 can hold records that define it as 110, and the value the
word then gives says which of the two forms KDOS loaded.

Both harnesses load KDOS without ``FSLOAD``, so ``BOOT-ID`` is the BIOS's
own identity on both, and one set of files serves them.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from shared.boot_identity import bios_code, boot_identity
from shared.token_form import (
    TokenWriter,
    pack_module_name_file,
    pack_token_file,
)

SKIPPED = object()
OTHER_SYSTEM = (
    "Token form recorded on another system: "
    "this volume was built for another system"
)
MALFORMED = "token stream is malformed"
MISMATCH = "token stream does not match this system"


def records(*items) -> tuple[bytes, int]:
    """Records for ITEMS: an int is a number, a string a word, a pair a
    word and the text it takes, "\n" the next source line, and SKIPPED the
    end of a conditional skip."""

    writer = TokenWriter()
    line = 1
    writer.line(line)
    for item in items:
        if item is SKIPPED:
            writer.skipped()
        elif item == "\n":
            line += 1
            writer.line(line)
        elif isinstance(item, int):
            writer.number(item)
        elif isinstance(item, tuple):
            name, text = item
            writer.word(name, name.encode(), text.encode())
        else:
            writer.word(item, item.encode())
    return writer.finish(), writer.slots


def token_file(
    source: bytes,
    items,
    *,
    boot_id: int | None = None,
    module: bytes = b"",
    beside: bool = False,
) -> bytes:
    """A token file that says it was recorded from SOURCE and holds the
    records of ITEMS, built for the system whose BOOT-ID is BOOT_ID: by
    default the one both harnesses boot."""

    recorded, slots = records(*items)
    return pack_token_file(
        recorded,
        slots,
        source=source,
        lines=source.count(b"\n"),
        module_name=module,
        boot_id=boot_identity() if boot_id is None else boot_id,
        source_beside=beside,
    )


# ---- the modules -----------------------------------------------------------

def module(name: str, value: int, *, requires: str | None = None) -> bytes:
    """The source of module NAME: it defines NAME-WORD as VALUE, or as one
    more than the word of the module it REQUIREs, and says when it loads."""

    upper = name.upper()
    lines = [f"PROVIDED mod-{name}"]
    if requires is None:
        lines.append(f": {upper}-WORD {value} ;")
    else:
        lines.append(f"REQUIRE {requires}.f")
        lines.append(f": {upper}-WORD {requires.upper()}-WORD 1+ ;")
    lines.append(f'." [load-{name}]"')
    return ("\n".join(lines) + "\n").encode()


def items(name: str, value: int, *, requires: str | None = None) -> tuple:
    """The records of module(NAME, VALUE, requires=REQUIRES)."""

    upper = name.upper()
    head = (("PROVIDED", f" mod-{name}"), "\n")
    if requires is None:
        body = ((":", f" {upper}-WORD"), value, ";")
    else:
        body = (
            ("REQUIRE", f" {requires}.f"), "\n",
            (":", f" {upper}-WORD"), f"{requires.upper()}-WORD", "1+", ";",
        )
    return head + body + ("\n", ('."', f' [load-{name}]"'))


def tokens(
    name: str,
    value: int,
    *,
    boot_id: int | None = None,
    says: int | None = None,
    requires: str | None = None,
    beside: bool = False,
) -> bytes:
    """Module NAME's token file.  With SAYS its header is that of the
    source that defines the word as SAYS, while its records define it as
    VALUE."""

    source = module(name, value if says is None else says, requires=requires)
    return token_file(
        source,
        items(name, value, requires=requires),
        boot_id=boot_id,
        module=f"mod-{name}".encode(),
        beside=beside,
    )


A = module("a", 11)
B = module("b", 0, requires="a")
C = module("c", 30)
D = module("d", 40)
E = module("e", 50)


def load(name: str, word: str = "REQUIRE") -> str:
    """A line that loads NAME and reports what the load threw."""

    return f"' {word} CATCH {name}  CR .\" [THROW \" . .\" ]\""


def show(*words: str) -> list[str]:
    """Lines that report whether each of WORDS is defined."""

    lines = []
    for word in words:
        lines.append(f'CR ." [DEF-{word} " [DEFINED] {word}-WORD . ." ]"')
    return lines


def value(word: str) -> str:
    return f'CR ." [{word} " {word}-WORD . ." ]"'


TOKENS_ON = "-1 MODULE-TOKENS !"


@dataclass
class Case:
    files: dict[str, bytes]
    lines: list[str]
    must: list[str] = field(default_factory=list)      # regular expressions
    must_not: list[str] = field(default_factory=list)  # plain text
    count: dict[str, int] = field(default_factory=dict)  # plain text: times


def _refused(file: str, throw: int, *more: str) -> list[str]:
    return [rf"Token file {file} cannot be loaded", rf"\[THROW {throw} \]", *more]


def _cases() -> dict[str, Case]:
    cases: dict[str, Case] = {}
    here = boot_identity()
    # Systems this one is not: a BIOS whose code differs in one byte, the
    # same BIOS after it loaded a KDOS from disk, and one bit of the cell.
    other_bios = bytearray(bios_code())
    other_bios[1000] ^= 0x01
    elsewhere = {
        "a BIOS that differs in one byte": boot_identity(bios=bytes(other_bios)),
        "a KDOS the BIOS loaded from disk": boot_identity(b"another KDOS\n"),
        "an identity that differs in one bit": here ^ (1 << 40),
        "no identity": 0,
    }
    assert here not in elsewhere.values()

    # ---- the loader's paths ------------------------------------------------
    cases["a volume of token files alone loads nested modules"] = Case(
        files={
            "a.ft": tokens("a", 11),
            "b.ft": tokens("b", 0, requires="a"),
        },
        lines=[
            TOKENS_ON, load("b.f"), value("B"), value("A"),
            'CR ." [MODS " MODULE? mod-a . MODULE? mod-b . ." ]"',
            load("a.f"), load("b.f"),
        ],
        must=[r"\[THROW 0 \]", r"\[B 12 \]", r"\[A 11 \]", r"\[MODS -1 -1 \]"],
        must_not=["not found", "cannot be loaded", "out of date"],
        count={"[load-a]": 1, "[load-b]": 1, "[THROW 0 ]": 3},
    )
    cases["a token file under its source's name loads by LOAD"] = Case(
        files={"a.f": tokens("a", 11)},
        lines=[load("a.f", "LOAD"), value("A")],
        must=[r"\[THROW 0 \]", r"\[A 11 \]"],
        must_not=["cannot be loaded"],
        count={"[load-a]": 1},
    )
    cases["source files load as before where no token form stands"] = Case(
        files={"a.f": A, "b.f": B},
        lines=[TOKENS_ON, load("b.f"), value("B")],
        must=[r"\[THROW 0 \]", r"\[B 12 \]"],
        must_not=["not found", "Token"],
        count={"[load-a]": 1, "[load-b]": 1},
    )
    cases["a module that has no file is reported as before"] = Case(
        files={},
        lines=[TOKENS_ON, load("nosuch.f")],
        must=[r"Module not found: nosuch\.f", r"\[THROW 0 \]"],
    )

    # A file that only names its module, for a module loaded earlier.
    named = {
        "a.ft": pack_module_name_file(b"mod-a"),
        "b.ft": tokens("b", 0, requires="a"),
    }
    cases["a file that only names a loaded module is passed over"] = Case(
        files=named,
        lines=[
            "PROVIDED mod-a", ": A-WORD 33 ;", TOKENS_ON, load("b.f"),
            value("B"), load("a.f"),
        ],
        must=[r"\[B 34 \]"],
        must_not=["not found", "only names", "cannot be loaded", "[load-a]"],
        count={"[THROW 0 ]": 2},
    )
    cases["a file that only names a module not loaded is refused"] = Case(
        files=named,
        lines=[TOKENS_ON, load("b.f"), *show("B", "A")],
        must=[
            r"Token file a\.ft only names its module, which is not loaded",
            r"\[THROW 7 \]", r"\[DEF-B 0 \]", r"\[DEF-A 0 \]",
        ],
        must_not=["[load-b]", "b.ft cannot"],
    )
    cases["LOAD of a file that only names a module is refused"] = Case(
        files=named,
        lines=["PROVIDED mod-a", load("a.ft", "LOAD")],
        must=[r"Token file a\.ft only names its module", r"\[THROW 7 \]"],
    )

    # ---- a file that cannot be replayed: statuses 6 and 7 ------------------
    good = tokens("a", 11)
    broken = {
        "cut short": good[:-3],
        "longer than its sizes say": good + b"\xff",
        "a header size below 64": good[:8] + (56).to_bytes(4, "little") + good[12:],
        "a header size that is not whole cells": (
            good[:8] + (76).to_bytes(4, "little") + good[12:]
        ),
        "a header size past the file": (
            good[:8] + (len(good) + 8).to_bytes(4, "little") + good[12:]
        ),
        "a module name past the header": (
            good[:44] + (200).to_bytes(2, "little") + good[46:]
        ),
        "a record count that is not the rest": (
            good[:16] + (5).to_bytes(8, "little") + good[24:]
        ),
    }
    for what, data in broken.items():
        cases[f"refused: a token file with {what}"] = Case(
            files={"a.ft": data},
            lines=[TOKENS_ON, load("a.f"), *show("A")],
            must=_refused(r"a\.ft", 6, r"\[DEF-A 0 \]"),
            must_not=["[load-a]"],
        )
    replayed = {
        "no END record": (records(*items("a", 11))[0][:-1], 6, MALFORMED),
        "an unknown opcode after a definition": (
            records(*items("a", 11))[0][:-1] + b"\xfa\xff", 6, MALFORMED,
        ),
        "a conditional that goes the other way": (
            records(*items("a", 11), 1, "[IF]", SKIPPED, 5, ".")[0],
            7, MISMATCH,
        ),
        "a word that takes entries out of the dictionary": (
            records(*items("a", 11), "HERE", "LATEST", "DICT-ROLLBACK")[0],
            7, MISMATCH,
        ),
        "a name that is not defined": (
            records(*items("a", 11), "NO-SUCH-WORD")[0], 1, r"\(not found\)",
        ),
    }
    for what, (recorded, status, message) in replayed.items():
        data = pack_token_file(
            recorded, 16, source=A, lines=3, module_name=b"mod-a", boot_id=here
        )
        named_too = status in (6, 7)
        cases[f"refused: records with {what}"] = Case(
            files={"a.ft": data},
            lines=[TOKENS_ON, load("a.f"), *show("A")],
            must=[
                message, rf"\[THROW {status} \]", r"\[DEF-A 0 \]",
                *([r"Token file a\.ft cannot be loaded"] if named_too else []),
            ],
            must_not=[] if named_too else ["cannot be loaded"],
        )
    cases["refused: a file found as a token form that is not one"] = Case(
        files={"a.ft": A},
        lines=[TOKENS_ON, load("a.f"), *show("A")],
        must=[r"Token file a\.ft is not a token file", r"\[THROW 7 \]",
              r"\[DEF-A 0 \]"],
        must_not=["[load-a]"],
    )
    cases["a refused file is named once, by the load that read it"] = Case(
        files={
            "a.ft": good[:-3],
            "b.ft": tokens("b", 0, requires="a"),
        },
        lines=[TOKENS_ON, load("b.f"), *show("B")],
        must=_refused(r"a\.ft", 6, r"\[DEF-B 0 \]"),
        must_not=["b.ft cannot", "[load-b]"],
        count={"cannot be loaded": 1},
    )

    # ---- the rule: built for this system -----------------------------------
    # No source stands beside these files, so one built for another system
    # ends the load.
    for what, identity in elsewhere.items():
        cases[f"refused: a file built for {what}"] = Case(
            files={"a.ft": tokens("a", 11, boot_id=identity)},
            lines=[TOKENS_ON, load("a.f"), *show("A")],
            must=[OTHER_SYSTEM, *_refused(r"a\.ft", 7, r"\[DEF-A 0 \]")],
            must_not=["[load-a]", "out of date"],
        )
    cases["refused: a file for another system ends the load that required it"] = Case(
        files={
            "a.ft": tokens("a", 11, boot_id=elsewhere["no identity"]),
            "b.ft": tokens("b", 0, requires="a"),
        },
        lines=[TOKENS_ON, load("b.f"), *show("B", "A")],
        must=[OTHER_SYSTEM,
              *_refused(r"a\.ft", 7, r"\[DEF-B 0 \]", r"\[DEF-A 0 \]")],
        must_not=["b.ft cannot"],
    )

    # ---- what the rule does not look at ------------------------------------
    # Each token file here gives ten times what its source would, so the
    # value says that the token file was used.
    lying_a = tokens("a", 110, says=11)
    cases["a token file is used though the file loaded before it has changed"] = Case(
        # b.f is source, and not the text it had when a.ft was built.
        files={"a.ft": lying_a, "b.f": B + b": B-LATER 1 ;\n"},
        lines=[TOKENS_ON, load("b.f"), value("B"), value("A")],
        must=[r"\[THROW 0 \]", r"\[B 111 \]", r"\[A 110 \]"],
        must_not=["cannot be loaded", "out of date"],
    )
    ordered = {"c.ft": tokens("c", 300, says=30), "d.ft": tokens("d", 400, says=40)}
    for what, first, second in (
        ("in one order", "c", "d"),
        ("in the other order", "d", "c"),
    ):
        cases[f"token files are used {what}"] = Case(
            files=ordered,
            lines=[TOKENS_ON, load(f"{first}.f"), load(f"{second}.f"),
                   value("C"), value("D")],
            must=[r"\[C 300 \]", r"\[D 400 \]"],
            must_not=["cannot be loaded"],
            count={"[THROW 0 ]": 2},
        )
    for what, script in (
        ("a script", b"-1 MODULE-TOKENS !\nREQUIRE a.f\n"),
        ("the same script after an edit",
         b": SCRIPT-WORD 9 ;\n-1 MODULE-TOKENS !\n\\ a line added later\nREQUIRE a.f\n"),
    ):
        cases[f"a token file is used when {what} loads it"] = Case(
            files={"outer.f": b"LOAD boot.f\n", "boot.f": script, "a.ft": lying_a},
            lines=[load("outer.f", "LOAD"), value("A")],
            must=[r"\[THROW 0 \]", r"\[A 110 \]"],
            must_not=["cannot be loaded"],
        )
    cases["a token file is used after a word typed and a source file loaded"] = Case(
        files={"s.f": b": S-WORD 7 ;\n", "a.ft": lying_a},
        lines=[TOKENS_ON, ": TYPED-WORD 5 ;", load("s.f", "LOAD"), load("a.f"),
               value("A"), 'CR ." [S " S-WORD TYPED-WORD + . ." ]"'],
        must=[r"\[A 110 \]", r"\[S 12 \]"],
        must_not=["cannot be loaded"],
        count={"[THROW 0 ]": 2},
    )

    # A token file that other code holds in memory and hands to the replay,
    # as a loader of containers does.
    for what, identity, shows in (
        ("is used when it was built for this system", here,
         [r"\[STATUS 0 \]", r"\[THROW 0 \]", r"\[DEF-A -1 \]"]),
        # The status ends the line that asked, and the load with it.
        ("is refused when it was built for another", elsewhere["no identity"],
         [OTHER_SYSTEM, r"\[THROW 7 \]", r"\[DEF-A 0 \]"]),
    ):
        held = tokens("a", 11, boot_id=identity)
        runner = ["CREATE HELD"]
        for index in range(0, len(held), 24):
            runner.append(
                " ".join(f"{byte} C," for byte in held[index : index + 24])
            )
        runner.append(
            f'HELD {len(held)} TOKEN-EVALUATE-CHECKED CR ." [STATUS " . ." ]"'
        )
        cases[f"a token file held in memory {what}"] = Case(
            files={"runner.f": ("\n".join(runner) + "\n").encode()},
            lines=[load("runner.f", "LOAD"), *show("A")],
            must=shows,
        )

    # ---- the rule: the source that ships beside the file -------------------
    def beside(name: str, source_value: int, *, text: bytes | None = None,
               boot_id: int | None = None):
        return {
            f"{name}.f": module(name, source_value) if text is None else text,
            f"{name}.ft": tokens(
                name, source_value * 10, says=source_value, beside=True,
                boot_id=boot_id,
            ),
        }

    cases["a current token file is used though its source stands beside it"] = Case(
        files=beside("a", 11),
        lines=[TOKENS_ON, load("a.f"), value("A")],
        must=[r"\[THROW 0 \]", r"\[A 110 \]"],
        must_not=["out of date", "cannot be loaded"],
    )
    for what, text, shows in (
        ("a source of another length", module("a", 4444), 4444),
        ("a source that differs in one byte", module("a", 12), 12),
    ):
        cases[f"{what} is loaded in its token file's place"] = Case(
            files=beside("a", 11, text=text),
            lines=[TOKENS_ON, load("a.f"), value("A")],
            must=[r"Token file a\.ft is out of date: loading its source",
                  r"\[THROW 0 \]", rf"\[A {shows} \]"],
            must_not=["cannot be loaded", OTHER_SYSTEM],
            count={"[load-a]": 1},
        )
    cases["a file built for another system gives way to the source beside it"] = Case(
        files=beside("a", 11, boot_id=elsewhere["a KDOS the BIOS loaded from disk"]),
        lines=[TOKENS_ON, load("a.f"), value("A")],
        must=[r"Token file a\.ft is out of date: loading its source",
              r"\[THROW 0 \]", r"\[A 11 \]"],
        must_not=["cannot be loaded", OTHER_SYSTEM],
    )
    three = {**beside("c", 30), **beside("d", 40), **beside("e", 50)}
    three_lines = [TOKENS_ON, load("c.f"), load("d.f"), load("e.f"),
                   value("C"), value("D"), value("E")]
    cases["current files with their sources beside them all load as tokens"] = Case(
        files=three, lines=three_lines,
        must=[r"\[C 300 \]", r"\[D 400 \]", r"\[E 500 \]"],
        must_not=["out of date"],
    )
    d_new = module("d", 41)
    cases["an edited module loads from its source and no other is affected"] = Case(
        files={**three, "d.f": d_new},
        lines=three_lines,
        must=[r"\[C 300 \]", r"\[D 41 \]", r"\[E 500 \]",
              r"Token file d\.ft is out of date: loading its source"],
        must_not=["c.ft is out of date", "e.ft is out of date", "cannot be loaded"],
        count={"is out of date: loading its source": 1},
    )
    cases["a token file built again for the edited module is used again"] = Case(
        files={
            **three,
            "d.f": d_new,
            "d.ft": tokens("d", 410, says=41, beside=True),
        },
        lines=three_lines,
        must=[r"\[C 300 \]", r"\[D 410 \]", r"\[E 500 \]"],
        must_not=["out of date"],
    )
    cases["a file that says its source is beside it loads without one"] = Case(
        files={"a.ft": tokens("a", 110, says=11, beside=True)},
        lines=[TOKENS_ON, load("a.f"), value("A")],
        must=[r"\[THROW 0 \]", r"\[A 110 \]"],
        must_not=["out of date"],
    )
    cases["refused: a file for another system whose source is said to be beside it and is not"] = Case(
        files={"a.ft": tokens(
            "a", 110, says=11, beside=True,
            boot_id=elsewhere["an identity that differs in one bit"],
        )},
        lines=[TOKENS_ON, load("a.f"), *show("A")],
        must=[OTHER_SYSTEM, *_refused(r"a\.ft", 7, r"\[DEF-A 0 \]")],
        must_not=["out of date"],
    )
    # In a folder, with a module REQUIREd by a path from the file's own.
    cases["a source beside its token file is found in the file's own folder"] = Case(
        files={
            "mods/b.f": B,
            "mods/b.ft": tokens("b", 0, requires="a", beside=True),
            "mods/a.f": module("a", 12),
            "mods/a.ft": tokens("a", 110, says=11, beside=True),
        },
        lines=[TOKENS_ON, load("mods/b.f"), value("B"), value("A")],
        must=[r"Token file a\.ft is out of date: loading its source",
              r"\[THROW 0 \]", r"\[B 13 \]", r"\[A 12 \]"],
        must_not=["b.ft is out of date", "cannot be loaded"],
    )
    return cases


CASES = _cases()


def volume(files: dict[str, bytes]):
    """An MP64FS boot volume that holds FILES; a name may lie in a folder."""

    from diskutil import FTYPE_FORTH, MP64FS

    fs = MP64FS(total_sectors=8192)
    fs.format()
    made = set()
    for path, data in files.items():
        folder, _, name = path.rpartition("/")
        if folder and folder not in made:
            fs.mkdir("/" + folder)
            made.add(folder)
        fs.inject_file(name, data, ftype=FTYPE_FORTH, path="/" + folder)
    return fs


def failures(case: Case, output: str) -> list[str]:
    """What OUTPUT, the UART text of CASE's lines, fails to show."""

    import re

    text = " ".join(output.split())
    found = []
    for pattern in case.must:
        if not re.search(pattern, text):
            found.append(f"missing: {pattern}")
    for plain in case.must_not:
        if plain in text:
            found.append(f"present: {plain}")
    for plain, times in case.count.items():
        if text.count(plain) != times:
            found.append(f"{plain!r} appears {text.count(plain)} times, not {times}")
    if found:
        found.append(f"output: {text[-900:]}")
    return found


# ---- files the simulator's recorder writes -----------------------------------

RECORDED_SOURCES = {
    "top.f": b"""PROVIDED rec-top
REQUIRE lib.f
\\ a comment line
VARIABLE T-V   ( a comment in parentheses )
: T-SQ  ( n -- n*n )  DUP * ;
7 T-SQ T-V !
: T-SAY  ." [top " T-V @ . ." ]" ;
1 [IF] : T-A 1 ; [ELSE] : T-A 2 ; [THEN]
0 [IF] : T-B 1 ; [ELSE] : T-B 2 ; [THEN]
[DEFINED] L-INC [IF] : T-C L-INC ; [THEN]
0 [IF]
: T-NO 1 ;
[THEN]
[DEFINED] T-NO [IF] : T-D 1 ; [ELSE] : T-D 0 ; [THEN]
CREATE T-TAB 10 , 20 , 30 ,
: T-AT  CELLS T-TAB + @ ;
-5 300 70000 1099511627776 + + + CONSTANT T-SUM
: T-STR  S" token text" ;
T-SAY
""",
    "lib.f": b"""PROVIDED rec-lib
: L-INC 1+ ;
: L-MAKER  CREATE , DOES> @ ;
41 L-MAKER L-41
." [lib]"
""",
}
RECORDED_LINES = [
    load("top.f"),
    'CR ." [VALUES " T-V @ . T-A . T-B . 5 T-C . 2 T-AT . T-SUM . T-D . L-41 . ." ]"',
    'CR ." [STR " T-STR TYPE ." ]"',
    'CR ." [MODS " MODULE? rec-top . MODULE? rec-lib . ." ]"',
]
RECORDED_SHOWS = [
    r"\[lib\]", r"\[top 49 \]", r"\[THROW 0 \]",
    r"\[VALUES 49 1 2 6 30 1099511698071 0 41 \]",
    r"\[STR token text\]", r"\[MODS -1 -1 \]",
]


def record(sources: dict[str, bytes], top: str):
    """Load TOP from a volume of SOURCES in the simulator with its recorder
    attached, and return the recorder and the load's UART output."""

    from pathlib import Path

    from simulator.runtime import MegaForthRuntime
    from simulator.source_acceleration import install_kdos_source_accelerators
    from simulator.storage import HostedStorageService
    from simulator.token_recorder import TokenRecorder

    kdos = (Path(__file__).resolve().parents[1] / "kdos.f").read_bytes()
    runtime = MegaForthRuntime(
        storage=HostedStorageService(bytearray(volume(sources).img))
    )
    recorder = TokenRecorder()
    recorder.attach(runtime)
    runtime.evaluate(kdos, source_name="kdos.f")
    install_kdos_source_accelerators(runtime)
    mark = len(runtime.uart_output)
    runtime.evaluate(load(top).encode(), source_name="record")
    return recorder, bytes(runtime.uart_output[mark:]).decode("latin-1")


def recorded_volume(sources: dict[str, bytes], top: str) -> dict[str, bytes]:
    """The volume of token files alone that a recording of TOP's load
    gives: each of SOURCES that was loaded, in its token form, under its
    name with a t added, built for the system both harnesses boot."""

    recorder, _ = record(sources, top)
    names = {text: name for name, text in sources.items()}
    return {
        names[unit.source] + "t": unit.token_file(boot_id=boot_identity())
        for unit in recorder.units
    }


__all__ = [
    "A", "B", "C", "D", "E", "CASES", "Case", "MALFORMED", "MISMATCH",
    "OTHER_SYSTEM", "RECORDED_LINES", "RECORDED_SHOWS", "RECORDED_SOURCES",
    "TOKENS_ON", "failures", "load", "module", "record", "recorded_volume",
    "records", "show", "token_file", "tokens", "value", "volume",
]
