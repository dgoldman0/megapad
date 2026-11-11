"""KDOS loading modules from token files: the cases both machines run.

``tests/test_system.py`` (``TestKDOSTokenModules``) runs each case on the
BIOS in the emulator, and ``tests/simulator/test_kdos_token_modules.py``
runs it in the hosted simulator.  A case is a set of files for the boot
volume, the lines typed at KDOS's prompt, what the output must show, and
what it must not.  Each case that ends "refused" plants one fault and holds
the loader to refusing the file, naming it, and loading nothing in its
place.

The token files are written by hand here, so that a file can state a
header its records do not match.  A file recorded from a text that defines
``A-WORD`` as 11 can hold records that define it as 22, and the value the
word then gives says which of the two forms KDOS loaded.

Both harnesses load KDOS without ``FSLOAD``, so ``BOOT-ID`` is the BIOS's
own identity on both, and one set of stamps serves them.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from shared.boot_identity import (
    boot_identity,
    stamp_start,
    stamp_take,
    text_identity,
)
from shared.token_form import (
    TokenWriter,
    pack_module_name_file,
    pack_token_file,
)

SKIPPED = object()
STALE = "Token form recorded on another system: this volume was built for another system"
MALFORMED = "token stream is malformed"
MISMATCH = "token stream does not match this system"


def records(*items) -> tuple[bytes, int]:
    """Records for ITEMS: an int is a number, a string a word, a pair a
    word and the text it takes, "\\n" the next source line, and SKIPPED the
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
    stamp: int,
    module: bytes = b"",
    beside: bool = False,
) -> bytes:
    """A token file that says it was recorded from SOURCE and holds the
    records of ITEMS."""

    recorded, slots = records(*items)
    return pack_token_file(
        recorded,
        slots,
        source=source,
        lines=source.count(b"\n"),
        module_name=module,
        stamp=stamp,
        source_beside=beside,
    )


def stamps(*texts: bytes, boot: int | None = None) -> list[int]:
    """The stamp of a load where each of TEXTS begins to load, in order,
    and last the stamp after all of them."""

    stamp = stamp_start(boot_identity() if boot is None else boot)
    result = []
    for text in texts:
        result.append(stamp)
        stamp = stamp_take(stamp, text_identity(text))
    result.append(stamp)
    return result


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
    stamp: int,
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
        stamp=stamp,
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
    """Lines that report whether each of WORDS is defined, and its value."""

    lines = []
    for word in words:
        lines.append(f'CR ." [DEF-{word} " [DEFINED] {word}-WORD . ." ]"')
    return lines


def value(word: str) -> str:
    return f'CR ." [{word} " {word}-WORD . ." ]"'


STAMP_LINE = (
    'CR ." [STAMP " LOAD-STAMP @ 32 RSHIFT . LOAD-STAMP @ 4294967295 AND . ." ]"'
)
TOKENS_ON = "-1 MODULE-TOKENS !"


def stamp_shown(stamp: int) -> str:
    return rf"\[STAMP {stamp >> 32} {stamp & 0xFFFFFFFF} \]"


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
    start = stamps()[0]
    s_ba = stamps(B, A)
    s_a = stamps(A)

    # ---- the loader's paths ------------------------------------------------
    cases["a volume of token files alone loads nested modules"] = Case(
        files={
            "a.ft": tokens("a", 11, stamp=s_ba[1]),
            "b.ft": tokens("b", 0, requires="a", stamp=s_ba[0]),
        },
        lines=[
            TOKENS_ON, load("b.f"), value("B"), value("A"),
            'CR ." [MODS " MODULE? mod-a . MODULE? mod-b . ." ]"',
            load("a.f"), load("b.f"), STAMP_LINE,
        ],
        must=[
            r"\[THROW 0 \]", r"\[B 12 \]", r"\[A 11 \]", r"\[MODS -1 -1 \]",
            stamp_shown(s_ba[2]),
        ],
        must_not=["not found", "cannot be loaded", "out of date"],
        count={"[load-a]": 1, "[load-b]": 1, "[THROW 0 ]": 3},
    )
    cases["a token file under its source's name loads by LOAD"] = Case(
        files={"a.f": tokens("a", 11, stamp=s_a[0])},
        lines=[load("a.f", "LOAD"), value("A"), STAMP_LINE],
        must=[r"\[THROW 0 \]", r"\[A 11 \]", stamp_shown(s_a[1])],
        must_not=["cannot be loaded"],
        count={"[load-a]": 1},
    )
    cases["a boot that meets no token file keeps no stamp"] = Case(
        files={"a.f": A, "b.f": B},
        lines=[TOKENS_ON, load("b.f"), value("B"), STAMP_LINE],
        must=[r"\[B 12 \]", r"\[STAMP 0 0 \]"],
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
        "b.ft": tokens("b", 0, requires="a", stamp=start),
    }
    cases["a file that only names a loaded module is passed over"] = Case(
        files=named,
        lines=[
            "PROVIDED mod-a", ": A-WORD 33 ;", TOKENS_ON, load("b.f"),
            value("B"), load("a.f"), STAMP_LINE,
        ],
        must=[r"\[B 34 \]", stamp_shown(stamps(B)[1])],
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
    good = tokens("a", 11, stamp=start)
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
            recorded, 16, source=A, lines=3, module_name=b"mod-a", stamp=start
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
            "b.ft": tokens("b", 0, requires="a", stamp=start),
        },
        lines=[TOKENS_ON, load("b.f"), *show("B")],
        must=_refused(r"a\.ft", 6, r"\[DEF-B 0 \]"),
        must_not=["b.ft cannot", "[load-b]"],
        count={"cannot be loaded": 1},
    )

    # ---- clauses 1 and 2: the stamp, where no source stands beside ---------
    def stale(what: str, files: dict[str, bytes], lines: list[str], *named: str):
        cases[f"refused: {what}"] = Case(
            files=files,
            lines=[TOKENS_ON, *lines],
            must=[STALE, r"\[THROW 7 \]",
                  *(rf"Token file {name} cannot be loaded" for name in named)],
            must_not=["out of date"],
        )

    stale(
        "a stamp that differs in one bit",
        {"a.ft": tokens("a", 11, stamp=start ^ (1 << 40))},
        [load("a.f"), *show("A")], r"a\.ft",
    )
    cases["refused: a stamp that differs in one bit"].must.append(r"\[DEF-A 0 \]")
    cases["refused: a stamp that differs in one bit"].must_not.append("[load-a]")
    stale(
        "a file that holds no stamp",
        {"a.ft": tokens("a", 11, stamp=0)},
        [load("a.f")], r"a\.ft",
    )
    stale(
        "a file recorded on another BIOS or KDOS",
        {"a.ft": tokens("a", 11, stamp=stamps(boot=boot_identity(b"other"))[0])},
        [load("a.f")], r"a\.ft",
    )
    b_old = B + b"\\ a line the file has since lost\n"
    stale(
        "a file recorded under an earlier text of the file before it",
        {
            "a.ft": tokens("a", 11, stamp=stamps(b_old, A)[1]),
            "b.ft": tokens("b", 0, requires="a", stamp=start),
        },
        [load("b.f"), *show("B", "A")], r"a\.ft",
    )
    case = cases["refused: a file recorded under an earlier text of the file before it"]
    case.must += [r"\[DEF-B 0 \]", r"\[DEF-A 0 \]"]
    case.must_not += ["b.ft cannot"]
    s_cd = stamps(C, D)
    ordered = {
        "c.ft": tokens("c", 30, stamp=s_cd[0]),
        "d.ft": tokens("d", 40, stamp=s_cd[1]),
    }
    stale(
        "files loaded in another order than they were recorded in",
        ordered, [load("d.f"), *show("D")], r"d\.ft",
    )
    cases["files load in the order they were recorded in"] = Case(
        files=ordered,
        lines=[TOKENS_ON, load("c.f"), load("d.f"), value("C"), value("D"),
               STAMP_LINE],
        must=[r"\[C 30 \]", r"\[D 40 \]", stamp_shown(s_cd[2])],
        must_not=["cannot be loaded"],
    )

    # The source files that are loading when the first token file is met
    # are part of the stamp, outermost first, and so is every source file
    # loaded after it.
    boot_f = b"-1 MODULE-TOKENS !\nREQUIRE a.f\n"
    outer_f = b"LOAD boot.f\n"
    s_oba = stamps(outer_f, boot_f, A)
    cases["the files that are loading when the stamp starts are part of it"] = Case(
        files={"outer.f": outer_f, "boot.f": boot_f,
               "a.ft": tokens("a", 11, stamp=s_oba[2])},
        lines=[load("outer.f", "LOAD"), value("A"), STAMP_LINE],
        must=[r"\[THROW 0 \]", r"\[A 11 \]", stamp_shown(s_oba[3])],
        must_not=["cannot be loaded"],
    )
    stale(
        "a file recorded without the script that loads it",
        {"outer.f": outer_f, "boot.f": boot_f,
         "a.ft": tokens("a", 11, stamp=stamps(boot_f, A)[1])},
        [load("outer.f", "LOAD"), *show("A")], r"a\.ft",
    )
    stale(
        "a file recorded under another text of the script that loads it",
        {"outer.f": outer_f, "boot.f": boot_f,
         "a.ft": tokens("a", 11, stamp=stamps(outer_f, boot_f + b"\n", A)[2])},
        [load("outer.f", "LOAD"), *show("A")], r"a\.ft",
    )
    plain = b": S-WORD 7 ;\n"
    s_asc = stamps(A, plain, C)
    cases["a source file loaded after the stamp started is part of it"] = Case(
        files={"a.ft": tokens("a", 11, stamp=s_asc[0]), "s.f": plain,
               "c.ft": tokens("c", 30, stamp=s_asc[2])},
        lines=[TOKENS_ON, load("a.f"), load("s.f", "LOAD"), load("c.f"),
               value("C"), STAMP_LINE],
        must=[r"\[C 30 \]", stamp_shown(s_asc[3])],
        must_not=["cannot be loaded"],
    )
    stale(
        "a file recorded under another text of a source file before it",
        {"a.ft": tokens("a", 11, stamp=s_asc[0]), "s.f": b": S-WORD 8 ;\n",
         "c.ft": tokens("c", 30, stamp=s_asc[2])},
        [load("a.f"), load("s.f", "LOAD"), load("c.f"), *show("C")], r"c\.ft",
    )

    # What the stamp leaves out: a source file whose load ended before the
    # first token file was met, and anything defined outside a file.  The
    # stamp starts at that token file with the files loading then, so
    # neither is in it.
    cases["a source file that ended before the first token file is in no stamp"] = Case(
        files={"s.f": plain, "a.ft": tokens("a", 11, stamp=start)},
        lines=[TOKENS_ON, load("s.f", "LOAD"), ": TYPED-WORD 5 ;", STAMP_LINE,
               load("a.f"), value("A"), 'CR ." [S " S-WORD TYPED-WORD + . ." ]"',
               STAMP_LINE],
        must=[r"\[STAMP 0 0 \]", r"\[A 11 \]", r"\[S 12 \]", stamp_shown(s_a[1])],
        must_not=["cannot be loaded"],
        count={"[THROW 0 ]": 2},
    )
    stale(
        "a file stamped as if an earlier, finished source file were in the stamp",
        {"s.f": plain, "a.ft": tokens("a", 11, stamp=stamps(plain, A)[1])},
        [load("s.f", "LOAD"), load("a.f"), *show("A")], r"a\.ft",
    )

    # A token file that other code holds in memory and hands to the replay,
    # as a loader of containers does: the source file whose line does so is
    # the innermost file of the load.  Its text holds the token file without
    # a stamp, and the stamp comes from a cell set at the prompt, so that the
    # text does not depend on its own checksum.
    held = tokens("a", 11, stamp=0)
    runner = ["CREATE HELD"]
    for index in range(0, len(held), 24):
        runner.append(" ".join(f"{byte} C," for byte in held[index : index + 24]))
    runner += [
        "T-STAMP @ HELD 48 + !",
        f'HELD {len(held)} TOKEN-EVALUATE-CHECKED CR ." [STATUS " . ." ]"',
    ]
    runner_f = ("\n".join(runner) + "\n").encode()
    s_ra = stamps(runner_f, A)
    for what, stamp, status in (
        ("is used under the stamp its holder's text gives", s_ra[1], 0),
        ("is refused without its holder's text in the stamp", start, 7),
    ):
        cases[f"a token file held in memory {what}"] = Case(
            files={"runner.f": runner_f},
            lines=["VARIABLE T-STAMP",
                   f"{stamp >> 32} 32 LSHIFT {stamp & 0xFFFFFFFF} OR T-STAMP !",
                   load("runner.f", "LOAD"), *show("A"), STAMP_LINE],
            must=(
                [r"\[STATUS 0 \]", r"\[THROW 0 \]", r"\[DEF-A -1 \]",
                 stamp_shown(s_ra[2])]
                if status == 0
                # The status ends the line that asked, and the load with it.
                else [STALE, r"\[THROW 7 \]", r"\[DEF-A 0 \]"]
            ),
        )
    cases["a source file a loader outside KDOS evaluates joins the stamp"] = Case(
        files={"a.ft": tokens("a", 11, stamp=start),
               "c.ft": tokens("c", 30, stamp=stamps(A, b"12345")[2])},
        lines=[TOKENS_ON, 'CREATE TXT 49 C, 50 C, 51 C, 52 C, 53 C,',
               "TXT 5 LOAD-STAMP-SOURCE", STAMP_LINE,
               load("a.f"), "TXT 5 LOAD-STAMP-SOURCE", load("c.f"), value("C")],
        must=[r"\[STAMP 0 0 \]", r"\[C 30 \]"],
        must_not=["cannot be loaded"],
    )

    # ---- clause 3: the source ships beside the token file ------------------
    # Each token file's records give ten times what its source gives.
    def beside(name: str, source_value: int, *, stamp: int, text: bytes | None = None):
        return {
            f"{name}.f": module(name, source_value) if text is None else text,
            f"{name}.ft": tokens(
                name, source_value * 10, says=source_value, stamp=stamp,
                beside=True,
            ),
        }

    cases["a current token file is used though its source stands beside it"] = Case(
        files=beside("a", 11, stamp=start),
        lines=[TOKENS_ON, load("a.f"), value("A"), STAMP_LINE],
        must=[r"\[THROW 0 \]", r"\[A 110 \]", stamp_shown(s_a[1])],
        must_not=["out of date", "cannot be loaded"],
    )
    for what, text, shows in (
        ("a source of another length", module("a", 4444), 4444),
        ("a source that differs in one byte", module("a", 12), 12),
    ):
        cases[f"{what} is loaded in its token file's place"] = Case(
            files=beside("a", 11, stamp=start, text=text),
            lines=[TOKENS_ON, load("a.f"), value("A"), STAMP_LINE],
            must=[r"Token file a\.ft is out of date: loading its source",
                  r"\[THROW 0 \]", rf"\[A {shows} \]",
                  stamp_shown(stamps(text)[1])],
            must_not=["cannot be loaded", STALE],
            count={"[load-a]": 1},
        )
    cases["a stale stamp loads the source that stands beside the file"] = Case(
        files=beside("a", 11, stamp=start ^ 1),
        lines=[TOKENS_ON, load("a.f"), value("A")],
        must=[r"Token file a\.ft is out of date: loading its source",
              r"\[THROW 0 \]", r"\[A 11 \]"],
        must_not=["cannot be loaded", STALE],
    )
    s_cde = stamps(C, D, E)
    three = {
        **beside("c", 30, stamp=s_cde[0]),
        **beside("d", 40, stamp=s_cde[1]),
        **beside("e", 50, stamp=s_cde[2]),
    }
    three_lines = [TOKENS_ON, load("c.f"), load("d.f"), load("e.f"),
                   value("C"), value("D"), value("E")]
    cases["current files with their sources beside them all load as tokens"] = Case(
        files=three, lines=three_lines,
        must=[r"\[C 300 \]", r"\[D 400 \]", r"\[E 500 \]"],
        must_not=["out of date"],
    )
    d_new = module("d", 41)
    cases["an edited module takes with it every module loaded after it"] = Case(
        files={**three, "d.f": d_new},
        lines=three_lines,
        must=[r"\[C 300 \]", r"\[D 41 \]", r"\[E 50 \]",
              r"Token file d\.ft is out of date", r"Token file e\.ft is out of date"],
        must_not=["c.ft is out of date", "cannot be loaded"],
        count={"is out of date: loading its source": 2},
    )
    s_cde_new = stamps(C, d_new, E)
    cases["token files built again for the edited module are used again"] = Case(
        files={
            **three,
            "d.f": d_new,
            "d.ft": tokens("d", 410, says=41, stamp=s_cde_new[1], beside=True),
            "e.ft": tokens("e", 500, says=50, stamp=s_cde_new[2], beside=True),
        },
        lines=three_lines,
        must=[r"\[C 300 \]", r"\[D 410 \]", r"\[E 500 \]"],
        must_not=["out of date"],
    )
    flagged = {"a.ft": tokens("a", 110, says=11, stamp=start, beside=True)}
    cases["a file that says its source is beside it loads without one"] = Case(
        files=flagged,
        lines=[TOKENS_ON, load("a.f"), value("A")],
        must=[r"\[THROW 0 \]", r"\[A 110 \]"],
        must_not=["out of date"],
    )
    stale(
        "a stale file whose source is said to be beside it and is not",
        {"a.ft": tokens("a", 110, says=11, stamp=start ^ 1, beside=True)},
        [load("a.f"), *show("A")], r"a\.ft",
    )
    # In a folder, with a module REQUIREd by a path from the file's own.
    s_ba = stamps(B, A)
    folder = {
        "mods/b.f": B,
        "mods/b.ft": tokens("b", 0, requires="a", stamp=s_ba[0], beside=True),
        "mods/a.f": module("a", 12),
        "mods/a.ft": tokens("a", 110, says=11, stamp=s_ba[1], beside=True),
    }
    cases["a source beside its token file is found in the file's own folder"] = Case(
        files=folder,
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
    STAMP_LINE,
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
    name with a t added."""

    recorder, _ = record(sources, top)
    names = {text: name for name, text in sources.items()}
    stamped = recorder.stamps(boot_identity())
    return {
        names[unit.source] + "t": unit.token_file(stamp=stamped[unit.index])
        for unit in recorder.units
    }

__all__ = [
    "A", "B", "C", "D", "E", "CASES", "Case", "MALFORMED", "MISMATCH", "RECORDED_LINES",
    "RECORDED_SHOWS", "RECORDED_SOURCES", "STALE", "TOKENS_ON", "failures",
    "module", "record", "recorded_volume", "records", "stamps", "token_file",
    "tokens", "volume",
]
