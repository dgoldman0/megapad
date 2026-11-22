"""KDOS loads uroko.f, Uroko's declarations of the base words, in the
hosted simulator.

``uroko.f`` declares the effects of BIOS and KDOS words in Uroko's form,
one ``EFFECT NAME ( effect )`` line to a word, for Uroko's checker to read.
``EFFECT`` parses the name and records nothing yet, so the module loads on
any system that runs KDOS; ``tests/test_uroko_module.py`` loads it on the
BIOS in the emulator and finds every word it declares there.  This file
also holds the declarations to the words that ``bios.asm`` and ``kdos.f``
define, which is how a renamed or misspelled word shows.
"""

from __future__ import annotations

import re
from pathlib import Path

from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.token_module_cases import volume

REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS = (REPOSITORY_ROOT / "kdos.f").read_bytes()
UROKO = (REPOSITORY_ROOT / "uroko.f").read_bytes()

_ENTRY_RE = re.compile(
    r'^\s*\.dq\s+\S+[^\n]*\n\s*\.db\s+(0x[0-9A-Fa-f]+|\d+)[^\n]*\n'
    r'\s*\.ascii\s+"((?:[^"\\]|\\.)*)"',
    re.MULTILINE,
)
_KDOS_DEFINITION_RE = re.compile(
    r"(?:^|\s)(?::|VARIABLE|2VARIABLE|CONSTANT|2CONSTANT|CREATE|DEFER|VALUE)\s+(\S+)",
    re.IGNORECASE,
)


def declared_names() -> list[str]:
    """The word each EFFECT line declares, in the file's order."""
    return re.findall(r"^EFFECT\s+(\S+)", UROKO.decode("utf-8"), re.MULTILINE)


def bios_names() -> set[str]:
    """Every entry in the BIOS dictionary, by its uppercased name."""
    asm = (REPOSITORY_ROOT / "bios.asm").read_text(encoding="utf-8")
    names = set()
    for flags, raw in _ENTRY_RE.findall(asm):
        name = raw.replace('\\"', '"').replace("\\\\", "\\")
        if (int(flags, 0) & 0x7F) == len(name.encode("utf-8")):
            names.add(name.upper())
    return names


def _run(lines: list[str]) -> str:
    runtime = MegaForthRuntime(
        storage=HostedStorageService(bytearray(volume({"uroko.f": UROKO}).img))
    )
    runtime.evaluate(KDOS, source_name="kdos.f")
    mark = len(runtime.uart_output)
    for line in lines:
        runtime.evaluate(line.encode(), source_name="case")
    assert runtime.main_context.returns.snapshot() == ()
    return bytes(runtime.uart_output[mark:]).decode("latin-1")


def test_every_declaration_names_a_bios_or_kdos_word() -> None:
    kdos_names = {
        name.upper() for name in _KDOS_DEFINITION_RE.findall(KDOS.decode("utf-8"))
    }
    names = declared_names()
    assert len(names) == len(set(names))
    assert [name for name in names
            if name.upper() not in bios_names() | kdos_names] == []


def test_kdos_loads_uroko_and_its_declarations_leave_the_stack_alone() -> None:
    output = _run([
        "7",
        "REQUIRE uroko.f",
        "MODULE? uroko.f .",
        "DEPTH . .",
        ": UROKO-AFTER  11 ;",
        "UROKO-AFTER .",
        "REQUIRE uroko.f",
        "DEPTH .",
    ])

    # Loaded once, with the 7 below it untouched; a second REQUIRE reads
    # nothing, and later definitions compile as before.
    assert " ".join(output.split()) == "-1 1 7 11 0"
