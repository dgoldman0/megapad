"""Unified mode selection preserves each server's configuration authority."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest

import megapad


ROOT = Path(__file__).resolve().parents[1]


def _cold_launch(
    arguments: list[str], *, block_servers: bool = False,
    legacy_script: str | None = None,
):
    blocked = ["_mp64_accel", "_megaforth_native", "session_server", "simulator_server"]
    if block_servers:
        blocked += ["emulator", "simulator", "hybrid"]
    launch = (
        "from megapad import main\nraise SystemExit(main(sys.argv[1:]))"
        if legacy_script is None
        else f"import runpy\nrunpy.run_path({str(ROOT / legacy_script)!r}, run_name='__main__')"
    )
    source = f"""
import importlib.abc
import sys

class BlockImports(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in {blocked!r}:
            raise AssertionError('unexpected import: ' + fullname)

sys.meta_path.insert(0, BlockImports())
{launch}
"""
    return subprocess.run(
        [sys.executable, "-c", source, *arguments],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=20,
    )


@pytest.mark.parametrize("help_option", ["-h", "--help"])
def test_top_level_help_requires_no_backend_or_native_import(help_option):
    result = _cold_launch([help_option], block_servers=True)
    assert result.returncode == 0, result.stderr
    assert "--mode {emulator,simulator,hybrid}" in result.stdout
    assert "default mode: emulator" in result.stdout
    assert "declared bounded integer machine routines" in " ".join(result.stdout.split())


@pytest.mark.parametrize("mode", ["unknown"])
def test_unavailable_mode_fails_before_importing_a_backend(mode):
    result = _cold_launch(["--mode", mode], block_servers=True)
    assert result.returncode == 2
    assert "invalid choice" in result.stderr
    assert mode in result.stderr
    assert "unexpected import" not in result.stderr


@pytest.mark.parametrize(
    ("mode_arguments", "module_name", "server_arguments"),
    [
        (
            ["--socket", "/tmp/example.sock", "--paused"],
            "emulator.server",
            ["--socket", "/tmp/example.sock", "--paused"],
        ),
        (
            ["--mode", "emulator", "--bios", "a bios.asm", "--lanes", "2"],
            "emulator.server",
            ["--bios", "a bios.asm", "--lanes", "2"],
        ),
        (
            ["--storage", "a disk.img", "--mode=simulator", "--paused"],
            "simulator.server",
            ["--storage", "a disk.img", "--paused"],
        ),
        (
            ["--mode", "simulator", "--help"],
            "simulator.server",
            ["--help"],
        ),
    ],
)
def test_launcher_forwards_arguments_and_return_code(
    monkeypatch, mode_arguments, module_name, server_arguments
):
    calls = []

    def main(arguments):
        calls.append(arguments)
        return 23

    def import_module(name):
        assert name == module_name
        return SimpleNamespace(main=main)

    original_arguments = list(mode_arguments)
    monkeypatch.setattr(megapad.importlib, "import_module", import_module)
    assert megapad.main(mode_arguments) == 23
    assert calls == [server_arguments]
    assert mode_arguments == original_arguments


@pytest.mark.parametrize(
    ("mode", "present", "absent"),
    [
        ("emulator", "--bios", "--semantic-step-budget"),
        ("simulator", "--semantic-step-budget", "--bios"),
        ("hybrid", "--hybrid-routines", "--bios"),
    ],
)
def test_selected_help_uses_backend_options_without_native_imports(
    mode, present, absent
):
    result = _cold_launch(["--mode", mode, "--help"])
    assert result.returncode == 0, result.stderr
    assert present in result.stdout
    assert absent not in result.stdout


@pytest.mark.parametrize(
    ("script", "present", "absent"),
    [
        ("session_server.py", "--bios", "--semantic-step-budget"),
        ("simulator_server.py", "--semantic-step-budget", "--bios"),
    ],
)
def test_deprecated_script_help_forwards_without_native_imports(
    script, present, absent,
):
    result = _cold_launch(["--help"], legacy_script=script)
    assert result.returncode == 0, result.stderr
    assert present in result.stdout
    assert absent not in result.stdout


@pytest.mark.parametrize(
    ("mode", "arguments", "rejected_option"),
    [
        (
            "emulator",
            ["--semantic-step-budget", "100"],
            "--semantic-step-budget",
        ),
        (
            "simulator",
            ["--storage", "absent.img", "--bios", "bios.asm"],
            "--bios",
        ),
    ],
)
def test_selected_server_rejects_other_backends_arguments(
    mode, arguments, rejected_option
):
    result = _cold_launch(["--mode", mode, *arguments])
    assert result.returncode == 2, result.stderr
    assert "unrecognized arguments" in result.stderr
    assert rejected_option in result.stderr
    assert "unexpected import" not in result.stderr
