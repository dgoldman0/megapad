"""Focused construction coverage for the semantic simulator server CLI."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys
import textwrap

import pytest

from shared.storage import (
    SECTOR_SIZE,
    STORAGE_RESULT_WRITE_PROTECTED,
    STORAGE_UNIT_TABLE,
)
from simulator.image_bootstrap import ImageBootstrapError
from simulator.memory import EXTERNAL_BASE, AddressClass
from simulator.session import SEMANTIC_QUANTUM_ENVIRONMENT
from simulator.server import build_argument_parser, prepare_server
from tests.simulator.test_image_bootstrap import _boot_image


def _region_sizes(prepared) -> dict[AddressClass, int]:
    return {
        region.kind: region.size
        for region in prepared.preparation.runtime.memory.regions
    }


def test_server_cli_builds_the_shared_semantic_facade(tmp_path, monkeypatch) -> None:
    now_ns = [7_000_000_000]
    epoch_ms = 1_788_890_400_000
    monkeypatch.setattr("simulator.server.time.time_ns", lambda: epoch_ms * 1_000_000)
    monkeypatch.setattr("simulator.server.time.monotonic_ns", lambda: now_ns[0])
    # The explicit option wins over the environment.
    monkeypatch.setenv(SEMANTIC_QUANTUM_ENVIRONMENT, "40000")
    image = tmp_path / "desktop-simulator.img"
    image.write_bytes(_boot_image())
    args = build_argument_parser().parse_args(
        [
            "--storage",
            str(image),
            "--socket",
            str(tmp_path / "simulator.sock"),
            "--ram-kib",
            "768",
            "--ext-mem-mib",
            "5",
            "--vram-mib",
            "2",
            "--cols",
            "96",
            "--rows",
            "32",
            "--semantic-step-budget",
            "10000",
            "--semantic-quantum-steps",
            "32768",
            "--paused",
        ]
    )

    prepared = prepare_server(args)
    try:
        assert prepared.preparation.boot_filename == b"kdos.f"
        assert prepared.machine.semantic_session.entry == (
            prepared.preparation.root_xt
        )
        assert prepared.machine.paused
        assert prepared.server.machine is prepared.machine
        assert prepared.server.socket_path == str(tmp_path / "simulator.sock")
        assert not (tmp_path / "simulator.sock").exists()
        session = prepared.machine.semantic_session
        assert session.semantic_step_budget == (
            10000 - prepared.preparation.autoexec_semantic_steps
        )
        assert (session.backend.geometry.cols, session.backend.geometry.rows) == (
            96, 32
        )
        assert session.semantic_quantum_steps == 32768
        runtime = prepared.preparation.runtime
        assert (runtime.rtc.uptime_ms, runtime.rtc.epoch_ms) == (0, epoch_ms)
        now_ns[0] += 50_000_000
        assert (runtime.rtc.uptime_ms, runtime.rtc.epoch_ms) == (50, epoch_ms + 50)
        for name, expected in (
            (b"AUTO-RUNS", 1),
            (b"AUTO-COLS", 96),
            (b"AUTO-ROWS", 32),
            (b"SESSION-RUNS", 0),
        ):
            word = runtime.find(name)
            assert word is not None
            assert runtime.memory.read64(word.body_address) == expected
        sizes = _region_sizes(prepared)
        assert sizes[AddressClass.BANK0] == 768 << 10
        assert sizes[AddressClass.EXTERNAL] == 5 << 20
        assert sizes[AddressClass.VRAM] == 2 << 20
        assert sizes[AddressClass.HBW] == 3 << 20
    finally:
        prepared.machine.stop()


def test_server_cli_rejects_unbound_entry_before_exposing_socket(tmp_path) -> None:
    image = tmp_path / "unbound-simulator.img"
    image.write_bytes(_boot_image(autoexec_body=b"1 AUTO-RUNS +!\n"))
    socket_path = tmp_path / "simulator.sock"
    args = build_argument_parser().parse_args(
        ["--storage", str(image), "--socket", str(socket_path)]
    )

    with pytest.raises(ImageBootstrapError, match="did not bind"):
        prepare_server(args)
    assert not socket_path.exists()


def test_server_cli_rejects_emulator_only_arguments_and_missing_images(
    tmp_path,
) -> None:
    parser = build_argument_parser()
    for option in ("--bios", "--nic-tap", "--audio", "--host-profile"):
        with pytest.raises(SystemExit):
            parser.parse_args(
                ["--storage", str(tmp_path / "missing.img"), option]
            )

    args = parser.parse_args(
        ["--storage", str(tmp_path / "missing.img")]
    )
    with pytest.raises(ValueError, match="storage image does not exist"):
        prepare_server(args)


def test_server_cli_attaches_each_storage_unit_to_its_own_image(tmp_path) -> None:
    """Each storage option attaches its image to its unit.  The settings
    disk and the system disk are attached write-protected: they refuse a
    write and their files stay as they were.  The user disk takes a write,
    and a flush brings its file up to it."""
    parser = build_argument_parser()
    paths = {}
    arguments = []
    for unit in STORAGE_UNIT_TABLE:
        paths[unit] = tmp_path / f"{unit.disk}.img"
        paths[unit].write_bytes(
            _boot_image()
            if unit.number == 0
            else bytes((0x30 + unit.number,)) * ((unit.number + 1) * SECTOR_SIZE)
        )
        arguments += [unit.option, str(paths[unit])]
    assert [unit.option for unit in STORAGE_UNIT_TABLE] == [
        "--storage", "--settings-storage", "--system-storage", "--user-storage",
    ]
    args = parser.parse_args(
        [*arguments, "--socket", str(tmp_path / "simulator.sock"), "--paused"]
    )
    before = {unit: path.read_bytes() for unit, path in paths.items()}

    prepared = prepare_server(args)
    try:
        runtime = prepared.preparation.runtime
        source = EXTERNAL_BASE + (64 << 20)
        payload = b"\xA7" * SECTOR_SIZE
        runtime.memory.write_bytes(source, payload)
        for unit in STORAGE_UNIT_TABLE:
            service = runtime.storage_units[unit.number]
            assert service is getattr(runtime, unit.host_name)
            assert service.present
            assert service.write_protected is unit.write_protected
            assert service.image_path == paths[unit].resolve()
            if unit.number == 0:
                continue
            assert service.image_bytes == before[unit]
            generation = service.media_generation
            written = service.write_checked(
                runtime.memory, source, 0, 1, generation=generation,
            )
            assert service.flush_checked(generation=generation) == 0
            if unit.write_protected:
                assert written == (0, STORAGE_RESULT_WRITE_PROTECTED)
                assert paths[unit].read_bytes() == before[unit]
            else:
                assert written == (1, 0)
                assert paths[unit].read_bytes() == (
                    payload + before[unit][SECTOR_SIZE:]
                )
    finally:
        prepared.machine.stop()


def test_server_cli_leaves_a_unit_without_an_option_empty(tmp_path) -> None:
    image = tmp_path / "boot.img"
    image.write_bytes(_boot_image())
    args = build_argument_parser().parse_args(
        ["--storage", str(image), "--socket", str(tmp_path / "simulator.sock"),
         "--paused"]
    )
    prepared = prepare_server(args)
    try:
        runtime = prepared.preparation.runtime
        assert [service.present for service in runtime.storage_units] == [
            unit.number == 0 for unit in STORAGE_UNIT_TABLE
        ]
    finally:
        prepared.machine.stop()


def test_server_cli_requires_every_given_storage_image_to_exist(tmp_path) -> None:
    """A missing image is an error for every unit, and the server creates
    none: a write-protected medium can never come to hold an image, and a
    session does not start on a disk nobody made."""
    image = tmp_path / "boot.img"
    image.write_bytes(_boot_image())
    missing = tmp_path / "missing.img"
    parser = build_argument_parser()
    for unit in STORAGE_UNIT_TABLE:
        arguments = [unit.option, str(missing)]
        if unit.number != 0:
            arguments += ["--storage", str(image)]
        with pytest.raises(
            ValueError, match=f"^{unit.label} image does not exist"
        ):
            prepare_server(parser.parse_args(arguments))
        assert not missing.exists()
    with pytest.raises(SystemExit):
        parser.parse_args(["--system-storage", str(image)])


def test_server_cli_rejects_invalid_quanta_before_preparing_the_image(
    tmp_path, monkeypatch
) -> None:
    image = tmp_path / "desktop-simulator.img"
    image.write_bytes(_boot_image())
    parser = build_argument_parser()
    for value in ("0", "-1", "fast"):
        with pytest.raises(SystemExit):
            parser.parse_args(
                ["--storage", str(image), "--semantic-quantum-steps", value]
            )

    def prepare_image_bootstrap(**_options):
        pytest.fail("an invalid quantum must fail before autoexec runs")

    monkeypatch.setattr(
        "simulator.server.prepare_image_bootstrap", prepare_image_bootstrap
    )
    monkeypatch.setenv(SEMANTIC_QUANTUM_ENVIRONMENT, "0")
    args = parser.parse_args(
        ["--storage", str(image), "--socket", str(tmp_path / "simulator.sock")]
    )
    with pytest.raises(ValueError, match=SEMANTIC_QUANTUM_ENVIRONMENT):
        prepare_server(args)


@pytest.mark.parametrize("executor", ["python", "auto"])
def test_explicit_executor_overrides_environment_without_mutating_it(
    tmp_path, monkeypatch, executor
) -> None:
    monkeypatch.setenv("MEGAFORTH_EXECUTOR", "native")
    # Explicit Python works without either extension; auto may choose Python
    # when the semantic extension is unavailable, despite the required-native
    # environment setting.
    monkeypatch.setitem(sys.modules, "_megaforth_native", None)
    image = tmp_path / "executor.img"
    image.write_bytes(_boot_image())
    args = build_argument_parser().parse_args(
        [
            "--storage", str(image),
            "--socket", str(tmp_path / "simulator.sock"),
            "--executor", executor,
            "--semantic-step-budget", "10000",
        ]
    )

    prepared = prepare_server(args)
    try:
        runtime = prepared.preparation.runtime
        assert runtime.execution_backend == "python"
        assert runtime.native_execution_stats["semantic_steps"] == 0
        auto_runs = runtime.find(b"AUTO-RUNS")
        assert auto_runs is not None
        assert runtime.memory.read64(auto_runs.body_address) == 1
        assert os.environ["MEGAFORTH_EXECUTOR"] == "native"
    finally:
        prepared.machine.stop()


def test_requested_native_executor_fails_before_boot_source_or_autoexec(
    tmp_path, monkeypatch
) -> None:
    monkeypatch.setenv("MEGAFORTH_EXECUTOR", "python")
    monkeypatch.setitem(sys.modules, "_megaforth_native", None)

    def evaluate_source(*_args, **_options):
        pytest.fail("an unavailable required executor must fail before source runs")

    monkeypatch.setattr(
        "simulator.image_bootstrap._evaluate_checked_source", evaluate_source
    )
    image = tmp_path / "executor.img"
    image.write_bytes(_boot_image())
    socket_path = tmp_path / "simulator.sock"
    args = build_argument_parser().parse_args(
        [
            "--storage", str(image),
            "--socket", str(socket_path),
            "--executor", "native",
        ]
    )

    with pytest.raises(RuntimeError, match="requires _megaforth_native"):
        prepare_server(args)
    assert not socket_path.exists()
    assert os.environ["MEGAFORTH_EXECUTOR"] == "python"


def test_python_image_session_runs_without_importing_the_emulator_accelerator(
    tmp_path,
) -> None:
    image = tmp_path / "python-only.img"
    image.write_bytes(_boot_image())
    socket_path = tmp_path / "simulator.sock"
    script = textwrap.dedent(
        """
        import importlib.abc
        import sys

        class BlockEmulatorAccelerator(importlib.abc.MetaPathFinder):
            def find_spec(self, fullname, path=None, target=None):
                if fullname == "_mp64_accel":
                    raise AssertionError("semantic session imported _mp64_accel")
                return None

        sys.meta_path.insert(0, BlockEmulatorAccelerator())
        from simulator.server import build_argument_parser, prepare_server

        args = build_argument_parser().parse_args([
            "--storage", sys.argv[1],
            "--socket", sys.argv[2],
            "--executor", "python",
            "--semantic-step-budget", "10000",
            "--semantic-quantum-steps", "1024",
        ])
        prepared = prepare_server(args)
        try:
            runtime = prepared.preparation.runtime
            assert runtime.execution_backend == "python"
            session = prepared.machine.semantic_session
            session.boot()
            result = session.run_boundary()
            assert result.semantic_steps > 0
            for name, expected in ((b"AUTO-RUNS", 1), (b"SESSION-RUNS", 1)):
                word = runtime.find(name)
                assert word is not None
                assert runtime.memory.read64(word.body_address) == expected
            assert runtime.main_context.data.snapshot() == ()
            assert runtime.main_context.returns.snapshot() == ()
            assert "_mp64_accel" not in sys.modules
            assert "emulator.system" not in sys.modules
        finally:
            prepared.machine.stop()
        """
    )
    completed = subprocess.run(
        [sys.executable, "-c", script, str(image), str(socket_path)],
        cwd=Path(__file__).resolve().parents[2],
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert completed.returncode == 0, completed.stdout + completed.stderr
    assert not socket_path.exists()
