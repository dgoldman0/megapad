"""Token streams that TOKEN-REPLAY's two implementations must take alike.

Each entry is ``name: (make, expected)``.  ``make()`` returns the records and
the table's slot count.  ``expected`` is what the replay prints, followed by
the report both test files append, with white space normalized::

    <output> status=<EVAL-STATUS> depth=<DEPTH> line=<EVAL-LINE>

``tests/simulator/test_token_replay.py`` runs them in the hosted simulator
and ``tests/test_system.py::TestBIOSTokenReplay`` on ``bios.asm``.
"""

from __future__ import annotations

from shared.token_form import TokenWriter

MALFORMED = "token stream is malformed"
MISMATCH = "token stream does not match this system"


def _definitions_numbers_and_text():
    w = TokenWriter()
    w.line(1)
    w.word("colon", b":", b" SQ")
    w.word("dup", b"DUP")
    w.word("star", b"*")
    w.word("semi", b";")
    w.line(2)
    w.number(7)
    w.word("sq", b"SQ")
    w.word("dot", b".")
    w.line(5)
    w.word("colon", b":", b" SAY")
    w.word("dotq", b'."', b' [hi]"')
    w.word("semi", b";")
    w.word("say", b"SAY")
    for value in (-5, 300, 70000, 1 << 40):
        w.number(value)
    for _ in range(4):
        w.word("dot", b".")
    # A comment is a word with no text: it only resets the compiler's fusing.
    w.word("paren", b"(")
    w.number(5)
    w.word("colon", b":", b" ADD5")
    w.number(5)
    w.word("plus", b"+")
    w.word("semi", b";")
    w.word("add5", b"ADD5")
    w.word("dot", b".")
    return w.finish(), w.slots


def _conditional_compilation():
    w = TokenWriter()
    w.line(1)
    w.number(0)
    w.word("if", b"[IF]")
    w.skipped()
    w.number(1)
    w.word("if", b"[IF]")
    w.number(42)
    w.word("dot", b".")
    w.word("then", b"[THEN]")
    w.number(1)
    w.word("if", b"[IF]")
    w.number(43)
    w.word("dot", b".")
    w.word("else", b"[ELSE]")
    w.skipped()
    w.number(44)
    w.word("dot", b".")
    w.word("defined", b"[DEFINED]", b" DUP")
    w.word("dot", b".")
    w.word("defined", b"[DEFINED]", b" NOSUCHWORD")
    w.word("dot", b".")
    return w.finish(), w.slots


def _recorded_with_a_skip_replayed_without():
    w = TokenWriter()
    w.number(1)
    w.word("if", b"[IF]")
    w.skipped()
    w.number(9)
    w.word("dot", b".")
    return w.finish(), w.slots


def _recorded_without_a_skip_replayed_with():
    w = TokenWriter()
    w.number(0)
    w.word("if", b"[IF]")
    w.number(9)
    w.word("dot", b".")
    return w.finish(), w.slots


def _a_slot_nothing_filled():
    return bytes([0xF1, 3, 0x00, 0x05, 0xFF]), 4


def _an_undefined_name():
    w = TokenWriter()
    w.number(1)
    w.word("missing", b"NOSUCHWORD")
    w.number(2)
    return w.finish(), w.slots


def _no_end_record():
    w = TokenWriter()
    w.number(1)
    w.word("dot", b".")
    return w.finish()[:-1], w.slots


def _more_imports_than_slots():
    w = TokenWriter()
    w.number(1)
    w.word("dot", b".")
    w.number(2)
    w.word("dup", b"DUP")
    return w.finish(), 1


def _words_bound_as_they_are_defined():
    w = TokenWriter()
    w.line(1)
    w.word("colon", b":", b" TWICE")
    w.word("dup", b"DUP")
    w.word("plus", b"+")
    w.word("semi", b";")
    w.defined("twice")
    w.word("variable", b"VARIABLE", b" V1")
    w.defined("v1")
    w.number(21)
    w.word("twice", b"TWICE")
    w.word("v1", b"V1")
    w.word("store", b"!")
    w.word("v1", b"V1")
    w.word("fetch", b"@")
    w.word("dot", b".")
    # The name defined again is another word; the first keeps its slot.
    w.word("colon", b":", b" TWICE")
    w.number(3)
    w.word("star", b"*")
    w.word("semi", b";")
    w.defined("thrice")
    w.number(5)
    w.word("thrice", b"TWICE")
    w.word("dot", b".")
    w.number(5)
    w.word("twice", b"TWICE")
    w.word("dot", b".")
    return w.finish(), w.slots


def _defined_with_a_full_table():
    w = TokenWriter()
    w.word("variable", b"VARIABLE", b" V2")
    w.defined("v2")
    return w.finish(), 1


def _a_text_that_passes_the_end():
    return bytes([0xF1, 1, 0xF5, 9, 0x20, 0x41]), 2


def _a_number_that_passes_the_end():
    return bytes([0xF1, 1, 0xF4, 1, 2, 3]), 2


def _an_unknown_opcode():
    return bytes([0xF1, 1, 0xFA, 0xFF]), 2


def _an_import_with_no_name():
    return bytes([0xF1, 1, 0xF0, 0, 0xFF]), 2


def _a_line_count_of_two_bytes():
    w = TokenWriter()
    w.line(1000)
    w.number(6)
    w.line(1001)
    w.word("dot", b".")
    return w.finish(), w.slots


STREAMS = {
    "definitions, numbers and text": (
        _definitions_numbers_and_text,
        "49 [hi]1099511627776 70000 300 -5 10 status=0 depth=0 line=5",
    ),
    "conditional compilation": (
        _conditional_compilation,
        "42 43 44 -1 0 status=0 depth=0 line=1",
    ),
    "recorded with a skip, replayed without": (
        _recorded_with_a_skip_replayed_without,
        f"{MISMATCH} status=7 depth=0 line=0",
    ),
    "recorded without a skip, replayed with": (
        _recorded_without_a_skip_replayed_with,
        f"{MISMATCH} status=7 depth=0 line=0",
    ),
    "a slot nothing filled": (
        _a_slot_nothing_filled,
        f"{MALFORMED} status=6 depth=1 line=0",
    ),
    "an undefined name": (
        _an_undefined_name,
        "NOSUCHWORD ? (not found) status=1 depth=1 line=0",
    ),
    "no END record": (
        _no_end_record,
        f"1 {MALFORMED} status=6 depth=0 line=0",
    ),
    "more imports than slots": (
        _more_imports_than_slots,
        f"1 {MALFORMED} status=6 depth=1 line=0",
    ),
    "words bound as they are defined": (
        _words_bound_as_they_are_defined,
        "42 15 10 status=0 depth=0 line=1",
    ),
    "DEFINED with a full table": (
        _defined_with_a_full_table,
        f"{MALFORMED} status=6 depth=0 line=0",
    ),
    "a text that passes the end": (
        _a_text_that_passes_the_end,
        f"{MALFORMED} status=6 depth=1 line=0",
    ),
    "a number that passes the end": (
        _a_number_that_passes_the_end,
        f"{MALFORMED} status=6 depth=1 line=0",
    ),
    "an unknown opcode": (
        _an_unknown_opcode,
        f"{MALFORMED} status=6 depth=1 line=0",
    ),
    "an import with no name": (
        _an_import_with_no_name,
        f"{MALFORMED} status=6 depth=1 line=0",
    ),
    "a line count of two bytes": (
        _a_line_count_of_two_bytes,
        "6 status=0 depth=0 line=1001",
    ),
}
