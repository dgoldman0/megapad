"""KDOS loads uroko.f, Uroko's declarations of the base words, on the BIOS.

``uroko.f`` declares the effects of BIOS and KDOS words in Uroko's form,
one ``EFFECT NAME ( effect )`` line to a word, for Uroko's checker to read.
Here the emulator boots the BIOS and KDOS, loads the module from a volume
with ``REQUIRE``, and finds every word the module declares in the
dictionary the BIOS and KDOS built.  ``tests/simulator/test_uroko_module.py``
loads it in the hosted simulator, which lacks a few of those BIOS words.
"""

from __future__ import annotations

import os
import re
import tempfile
from pathlib import Path

from tests.test_system import _KDOSTestBase
from tests.token_module_cases import volume

REPOSITORY_ROOT = Path(__file__).resolve().parents[1]
UROKO = (REPOSITORY_ROOT / "uroko.f").read_bytes()


class TestUrokoModule(_KDOSTestBase):
    """The module loads under KDOS and names only words the BIOS has."""

    def _run_volume(self, lines: list[str]) -> str:
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            volume({"uroko.f": UROKO}).save(path)
            return self._run_kdos(lines, storage_image=path)
        finally:
            os.unlink(path)

    def test_kdos_loads_uroko_and_finds_every_word_it_declares(self):
        names = re.findall(r"^EFFECT\s+(\S+)", UROKO.decode("utf-8"), re.MULTILINE)
        # Each result is printed plus a constant, so the numbers below
        # appear in no echoed input line.  A word ' cannot find leaves 0,
        # and the count of words found then falls short of the declarations.
        lines = [
            "7",
            "REQUIRE uroko.f",
            "MODULE? uroko.f 1000 + .",
            "DEPTH 2000 + .",
            "0",
            *(f"' {name} 0<> -" for name in names),
            "3000 + .",
            "2000 + .",
        ]
        output = self._run_volume(lines)
        # Loaded, with the 7 below it untouched; every declared word found.
        for printed in ("999", "2001", str(3000 + len(names)), "2007"):
            self.assertRegex(output, rf"(?<![\d-]){printed}(?!\d)")
