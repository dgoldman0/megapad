"""Lightweight structural locks for guest retained discovery composition."""

from __future__ import annotations

import re
from pathlib import Path


SOURCE = Path(__file__).resolve().parents[1] / "presentation-terminal.f"


def _definition(source: str, word: str) -> str:
    match = re.search(
        rf"^:\s+{re.escape(word)}(?:\s|$).*?;\s*$",
        source,
        re.MULTILINE | re.DOTALL,
    )
    assert match is not None, word
    return match.group(0)


def test_retained_discovery_is_explicit_and_scheduled_without_input_starvation() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    discover = _definition(source, "PT-RETAINED-DISCOVER")
    service = _definition(source, "PT-SERVICE")

    assert "_PT.S.RET-ENABLED? !" in discover
    assert "_PT-FRAME" not in discover
    assert service.count("_PT-SERVICE-RET-QUERY") == 2
    assert service.index("_PT-SERVICE-RET-QUERY") < service.index(
        "_PT-SERVICE-BINARY"
    ) < service.rindex("_PT-SERVICE-RET-QUERY")


def test_retained_records_and_legacy_snapshot_are_lifecycle_gated() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    caps = _definition(source, "PT-RETAINED-CAPS@")
    formats = _definition(source, "PT-RETAINED-FORMATS@")
    begin = _definition(source, "_PT-BEGIN-TX")

    assert "PT-RETAINED-AVAILABLE?" in caps
    assert "PT-RETAINED-AVAILABLE?" in formats
    assert "_PT.S.RET-STATE @ _PT-RD-AVAILABLE =" in begin
    assert "PT-S-UNSUPPORTED EXIT" in begin
