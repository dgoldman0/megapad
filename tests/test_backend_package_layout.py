"""Structural guards for emulator/shared/simulator ownership."""

from __future__ import annotations

import ast
import importlib
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def test_flat_machine_imports_alias_canonical_emulator_modules() -> None:
    pairs = (
        ("megapad64", "emulator.megapad64"),
        ("accel_wrapper", "emulator.accel_wrapper"),
        ("devices", "emulator.devices"),
        ("system", "emulator.system"),
        ("rich_terminal.megapad", "emulator.rich_terminal_host"),
    )

    for flat_name, canonical_name in pairs:
        flat = importlib.import_module(flat_name)
        canonical = importlib.import_module(canonical_name)
        assert flat is canonical


def test_shared_and_backends_obey_the_dependency_direction() -> None:
    forbidden = {
        "shared": {"emulator", "simulator"},
        "simulator": {"emulator"},
        "emulator": {"simulator"},
    }

    for package, forbidden_roots in forbidden.items():
        for path in sorted((ROOT / package).rglob("*.py")):
            tree = ast.parse(path.read_bytes(), filename=str(path))
            imports: set[str] = set()
            for node in ast.walk(tree):
                if isinstance(node, ast.Import):
                    imports.update(alias.name.partition(".")[0] for alias in node.names)
                elif isinstance(node, ast.ImportFrom) and node.level == 0:
                    if node.module:
                        imports.add(node.module.partition(".")[0])

            violations = sorted(imports & forbidden_roots)
            assert not violations, f"{path.relative_to(ROOT)} imports {violations}"


def test_generic_rich_terminal_surface_does_not_export_machine_adapter() -> None:
    rich_terminal = importlib.import_module("rich_terminal")

    assert not hasattr(rich_terminal, "MegapadRichTerminalHost")

