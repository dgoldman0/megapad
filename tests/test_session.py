"""Focused tests for the native UART and headless development session."""

from __future__ import annotations

import json
from pathlib import Path
from unittest.mock import patch

import pytest

from cli import MegapadCLI, main as cli_main
from dev_session import run_scenario
from devices import UART
from display import VirtualTerminal
from nic_backends import LoopbackBackend
from presentation_terminal import (
    Cell,
    Cursor,
    DriverLimits,
    DriverStatus,
    EgressWatermarks,
    HostPortLimits,
    TerminalConfig,
    TerminalSessionError,
    TerminalState,
    TerminalView,
)
from session import (
    MachineSession,
    PresentationSessionConfig,
    PresentationSessionPolicy,
)
from session_server import main as session_server_main
from system import EXT_MEM_BASE, HBW_BASE, VRAM_BASE, MegapadSystem


ROOT = Path(__file__).resolve().parents[1]
BIOS = ROOT / "bios.asm"


def _presentation_config(*, ansi_history_bytes: int = 32) -> PresentationSessionConfig:
    return PresentationSessionConfig(
        host_limits=HostPortLimits(
            egress=EgressWatermarks(8_192, 1_024, 16, 2),
            retained_publication_bytes=4_608,
            ingress_bytes=8_192,
            ingress_events=16,
            ingress_control_bytes=4_096,
            ingress_control_events=8,
            geometry_events=2,
        ),
        terminal_config=TerminalConfig(
            max_payload=256,
            max_transaction_bytes=512,
            terminal_receive_credit=1_024,
            max_cells=16,
            max_feed_bytes=4_608,
            cols=2,
            rows=2,
        ),
        driver_limits=DriverLimits(4_096, 8),
        ansi_history_bytes=ansi_history_bytes,
        service_batches=2,
    )


def _presentation_policy() -> PresentationSessionPolicy:
    return PresentationSessionPolicy(
        max_cols=400,
        max_rows=200,
        egress_high_publications=2,
        egress_high_batches=32,
        egress_low_batches=4,
        ingress_bytes=128 * 1024,
        ingress_events=256,
        ingress_control_bytes=4_096,
        ingress_control_events=32,
        geometry_events=8,
        pending_outbound_bytes=128 * 1024,
        pending_outbound_events=256,
        ansi_history_bytes=256 * 1024,
        service_batches=4,
    )


def _drain_uart_rx(system: MegapadSystem) -> bytes:
    result = bytearray()
    while system.uart.has_rx_data:
        result.append(system.cpu._cs.uart_read8(0x01))
    return bytes(result)


def test_native_uart_rx_status_and_batched_tx():
    system = MegapadSystem(ram_size=64 * 1024)
    system.uart.inject_input("Aé")
    state = system.cpu._cs

    assert state.uart_read8(0x02) & 0x02
    assert state.uart_read8(0x01) == ord("A")
    assert state.uart_read8(0x01) == 0xC3
    assert state.uart_read8(0x01) == 0xA9
    assert not (state.uart_read8(0x02) & 0x02)

    batches = []
    system.uart.on_tx_batch = batches.append
    state.uart_write8(0x00, ord("O"))
    state.uart_write8(0x00, ord("K"))
    assert system._drain_native_uart_output() == b"OK"
    assert batches == [b"OK"]


def test_native_uart_drains_bios_ring_once():
    system = MegapadSystem(ram_size=64 * 1024)
    ring = 0x1000
    payload = b"one terminal batch"
    system.cpu.mem[ring:ring + 8] = len(payload).to_bytes(8, "little")
    system.cpu.mem[ring + 8:ring + 8 + len(payload)] = payload
    system.uart._tx_ring_base = ring

    listener_calls = []
    system.uart._tx_listeners.append(listener_calls.append)
    system.cpu._cs.uart_write8(0x06, 1)
    system._drain_native_uart_output()

    assert listener_calls == [payload]
    assert int.from_bytes(system.cpu.mem[ring:ring + 8], "little") == 0


def test_multicore_uses_one_shared_uart_owner():
    system = MegapadSystem(ram_size=64 * 1024, num_cores=2)
    assert system.cores[0]._cs.uart_enabled()
    assert system.cores[1]._cs.uart_enabled()
    system.uart.inject_input(b"X")
    # Every full core reaches the one SystemState-owned native queue.
    assert system.cores[1]._cs.uart_read8(0x01) == ord("X")


def test_reference_uart_listener_preserves_ring_batch():
    uart = UART()
    uart._cpu_mem = bytearray(0x2000)
    uart._tx_ring_base = 0x1000
    payload = b"batch"
    uart._cpu_mem[0x1000:0x1008] = len(payload).to_bytes(8, "little")
    uart._cpu_mem[0x1008:0x1008 + len(payload)] = payload
    calls = []
    uart._tx_listeners.append(calls.append)

    uart.write8(0x06, 1)

    assert calls == [payload]


def test_load_binary_uses_regions_and_preserves_ram_wrap():
    system = MegapadSystem(
        ram_size=64,
        hbw_size=64,
        ext_mem_size=64,
        vram_size=64,
    )
    system.load_binary(62, b"ABCD")
    assert bytes(system.cpu.mem[62:64]) == b"AB"
    assert bytes(system.cpu.mem[0:2]) == b"CD"

    system.load_binary(HBW_BASE + 4, b"HBW")
    system.load_binary(EXT_MEM_BASE + 5, b"EXT")
    system.load_binary(VRAM_BASE + 6, b"VRAM")
    assert bytes(system._hbw_mem[4:7]) == b"HBW"
    assert bytes(system._ext_mem[5:8]) == b"EXT"
    assert bytes(system._vram_mem[6:10]) == b"VRAM"


def test_virtual_terminal_batch_equivalence_and_resize():
    data = b"hello\x1b[31m red\x1b[0m \xe2\x98\x85"
    batched = VirtualTerminal(cols=30, rows=5)
    per_byte = VirtualTerminal(cols=30, rows=5)
    batched.write(data)
    for value in data:
        per_byte.write(value)
    assert batched.grid == per_byte.grid

    before = batched.grid[0][:]
    batched.resize(40, 8)
    assert batched.grid[0][:30] == before
    assert len(batched.grid) == 8
    assert all(len(row) == 40 for row in batched.grid)
    batched.resize(10, 2)
    assert len(batched.grid) == 2
    assert all(len(row) == 10 for row in batched.grid)


def test_machine_session_named_edit_keys_use_terminal_sequences():
    system = MegapadSystem(ram_size=64 * 1024)
    with MachineSession(system) as session:
        session.send_key("backspace")
        session.send_key("delete")
        state = system.cpu._cs
        received = bytes(state.uart_read8(0x01) for _ in range(5))

    assert received == b"\x08\x1b[3~"


def test_machine_session_encodes_modified_named_characters():
    system = MegapadSystem(ram_size=64 * 1024)
    with MachineSession(system) as session:
        session.send_key("ctrl+space")
        state = system.cpu._cs
        received = bytes(state.uart_read8(0x01) for _ in range(7))

    assert received == b"\x1b[32;5u"


def test_machine_session_encodes_alt_character_shortcut():
    system = MegapadSystem(ram_size=64 * 1024)
    with MachineSession(system) as session:
        session.send_key("alt+5")
        state = system.cpu._cs
        received = bytes(state.uart_read8(0x01) for _ in range(2))

    assert received == b"\x1b5"


def test_machine_session_encodes_modified_named_navigation_keys():
    system = MegapadSystem(ram_size=64 * 1024)
    with MachineSession(system) as session:
        session.send_key("alt+left")
        session.send_key("ctrl+pagedown")
        session.send_key("alt+delete")
        state = system.cpu._cs
        received = bytes(state.uart_read8(0x01) for _ in range(18))

    assert received == b"\x1b[1;3D\x1b[6;5~\x1b[3;3~"


def test_machine_session_optional_terminal_owns_preswitch_input_and_geometry():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        batch_steps=1,
        presentation=_presentation_config(),
    ) as session:
        assert session.presentation_enabled
        assert session.presentation_state is TerminalState.ANSI
        assert system.presentation_terminal_host.enhanced_attached
        assert system.presentation_terminal_host.pending_geometry_events == 1
        assert session.send_text("boot\r") is DriverStatus.PROGRESS
        assert system.uart.rx_pending == 0

        system.cpu.halted = True
        boundary = session.run_batch_stats(1)
        assert boundary.external_events_applied == 2
        assert _drain_uart_rx(system) == b"boot\r"


def test_machine_session_bounds_optional_ansi_history_without_losing_screen():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=_presentation_config(ansi_history_bytes=4),
    ) as session:
        payload = b"\x1b[31mA"
        for value in payload:
            system.cpu._cs.uart_write8(0x00, value)
        assert system._drain_native_uart_output() == payload
        serviced = session.service_presentation_terminal()
        assert serviced is not None and serviced.ansi_bytes == len(payload)
        assert bytes(session.raw_output) == b"31mA"
        assert (session.raw_output_start, session.raw_output_end) == (2, 6)
        snapshot = session.snapshot()
        assert snapshot.lines()[0].startswith("A")
        assert snapshot.cells[0][0].fg == VirtualTerminal.COLORS[1]
        session.reset()
        assert bytes(session.raw_output) == b""
        assert (session.raw_output_start, session.raw_output_end) == (6, 6)


def test_machine_session_reset_replaces_the_optional_attachment_epoch():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=_presentation_config(),
    ) as session:
        session.boot()
        first = session.presentation_driver
        first_epoch = first.attachment_epoch
        first.close()
        system.cpu.halted = True
        assert session.run(max_steps=1).reason == "terminal_failure"
        assert "became stale" in session.presentation_failure
        assert session.presentation_lost
        assert session.presentation_state is TerminalState.FAILED
        assert session.send_text("blocked") is DriverStatus.FAILED
        assert session.send_key("enter") is DriverStatus.FAILED
        assert session.resize(4, 1) is DriverStatus.FAILED
        session.reset()
        second = session.presentation_driver
        assert second is not None
        assert second.attachment_epoch > first_epoch
        assert not session.presentation_lost
        assert session.presentation_state is TerminalState.ANSI
        assert system.presentation_terminal_host.pending_geometry_events == 1


def test_machine_session_failed_warm_boot_cannot_fall_through_to_legacy(
    monkeypatch,
):
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=_presentation_config(),
    ) as session:
        session.boot()

        def fail_boot(*args, **kwargs):
            raise RuntimeError("injected boot failure")

        monkeypatch.setattr(system, "boot", fail_boot)
        with pytest.raises(RuntimeError, match="injected boot failure"):
            session.boot()

        assert session.presentation_driver is None
        assert session.presentation_lost
        assert "presentation boot failed" in session.presentation_failure
        assert session.send_text("must not become raw") is DriverStatus.FAILED
        assert session.send_key("enter") is DriverStatus.FAILED
        assert session.resize(4, 1) is DriverStatus.FAILED
        assert session.run(max_steps=1).reason == "terminal_failure"
        with pytest.raises(TerminalSessionError, match="presentation boot failed"):
            session.step()


def test_machine_session_optional_attach_failure_restores_uart_callbacks():
    system = MegapadSystem(ram_size=64 * 1024)
    byte_callback = lambda value: None
    batch_callback = lambda data: None
    system.uart.on_tx = byte_callback
    system.uart.on_tx_batch = batch_callback
    system.schedule_terminal_resize(80, 30, at_cycle=1_000)

    with pytest.raises(RuntimeError, match="pending legacy terminal input"):
        MachineSession(
            system,
            cols=2,
            rows=2,
            presentation=_presentation_config(),
        )

    assert system.uart.on_tx is byte_callback
    assert system.uart.on_tx_batch is batch_callback
    assert not system.presentation_terminal_host.enhanced_attached


def test_machine_session_presents_cell_views_with_wire_attribute_mapping():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=_presentation_config(),
    ) as session:
        system.cpu.halted = True
        session.run_batch_stats(1)  # Cross the initial geometry boundary.
        view = TerminalView(
            attachment_epoch=session.presentation_driver.attachment_epoch,
            session_id=7,
            presentation_epoch=1,
            revision=1,
            cols=2,
            rows=2,
            cells=(
                (Cell(ord("A"), 1, 2, 0x40), Cell(ord("B"), 3, 4, 0x20)),
                (Cell(ord("C"), 5, 6, 0x08), Cell(ord("D"), 7, 0, 0)),
            ),
            dirty_spans=(),
            cursor=Cursor(1, 1, True),
        )
        session._receive_presentation_view(view)
        snapshot = session.snapshot()

        assert snapshot.lines() == ["AB", "CD"]
        assert snapshot.cells[0][0].fg == VirtualTerminal.COLORS[1]
        assert snapshot.cells[0][0].bg == VirtualTerminal.COLORS[2]
        assert snapshot.cells[0][0].attrs == 0x80
        assert snapshot.cells[0][1].attrs == 0x20
        assert (snapshot.cursor_col, snapshot.cursor_row) == (1, 1)

        # A committed resize can be followed by CLOSE before its replacement
        # snapshot.  Keep showing the last immutable view while synchronizing
        # the hidden ANSI fallback to the core's already-selected geometry.
        before_sync = session.revision
        session.presentation_driver.core.select_ansi_geometry(4, 1)
        session._sync_presentation_geometry()
        assert (session.terminal.cols, session.terminal.rows) == (4, 1)
        assert session.snapshot().lines() == ["AB", "CD"]
        assert session.revision == before_sync
        session._refresh_presentation_display_boundary()
        assert (session.snapshot().cols, session.snapshot().rows) == (4, 1)
        assert session.revision == before_sync + 1


def test_machine_session_can_advance_timer_while_guest_is_idle():
    system = MegapadSystem(ram_size=64 * 1024)
    system.cpu.idle = True
    system.cpu.flag_i = True
    system.timer.counter = 0
    system.timer.compare = 100
    system.timer.control = 0x03
    system.run_batch = lambda count: count

    with MachineSession(system) as session:
        report = session.run(
            max_steps=1,
            wall_timeout_s=0.1,
            advance_idle=True,
            idle_tick_cycles=100,
        )

    assert system.timer.irq_pending
    assert report.reason == "step_budget"


def test_machine_session_boots_interacts_and_captures(tmp_path):
    with MachineSession.from_bios(BIOS, cols=80, rows=30) as session:
        assert session.system.ext_mem_size == 128 << 20
        session.boot()
        boot = session.wait_for_idle(max_steps=2_000_000)
        assert boot.reason == "idle"
        assert "Megapad-64 Forth BIOS" in session.raw_text()

        session.clear_output()
        session.send_text("6 7 * .\n")
        result = session.wait_for_text("42 ", max_steps=2_000_000)
        assert result.matched

        snapshot = session.snapshot()
        assert snapshot.find("42")
        assert session.output_batches > 0
        assert session.output_byte_callbacks == 0

        text_path = tmp_path / "screen.txt"
        json_path = tmp_path / "screen.json"
        png_path = tmp_path / "screen.png"
        snapshot.write_text(text_path)
        snapshot.write_json(json_path)
        snapshot.write_png(png_path)
        assert "42" in text_path.read_text(encoding="utf-8")
        assert json.loads(json_path.read_text())["cursor"]["visible"]
        assert png_path.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
        from PIL import Image
        bounds = Image.open(png_path).getbbox()
        assert bounds is not None
        assert bounds[2] - bounds[0] > 100
        assert bounds[3] - bounds[1] > 40


def test_machine_session_owns_injected_nic_backend():
    backend = LoopbackBackend()

    with MachineSession.from_bios(BIOS, nic_backend=backend) as session:
        assert session.system._nic_backend is backend
        assert backend.link_up

    assert not backend.link_up


def test_cli_ramsize_preserves_machine_configuration(tmp_path, capsys):
    image = tmp_path / "configured.img"
    image.write_bytes(bytes(1024))
    backend = LoopbackBackend()
    system = MegapadSystem(
        ram_size=64 * 1024,
        storage_image=str(image),
        nic_backend=backend,
        num_cores=2,
        num_clusters=1,
        hbw_size=128,
        ext_mem_size=2 << 20,
        vram_size=256,
        realtime_clock=True,
        worker_count=1,
    )
    cli = MegapadCLI(system)

    cli.do_ramsize("128")

    assert cli.sys is not system
    assert cli.sys.ram_size == 128 * 1024
    assert cli.sys.storage.image_path == str(image)
    assert cli.sys._nic_backend is backend
    assert backend.link_up
    assert cli.sys.num_full_cores == 2
    assert cli.sys.num_clusters == 1
    assert cli.sys.hbw_size == 128
    assert cli.sys.ext_mem_size == 2 << 20
    assert cli.sys.vram_size == 256
    assert cli.sys.rtc.realtime
    assert cli.sys.worker_count == 1
    assert cli.sys.uart.on_tx == cli._uart_tx_handler
    assert "RAM resized to 128 KiB" in capsys.readouterr().out

    cli.sys.nic.stop()


def test_machine_session_close_persists_attached_storage(tmp_path):
    image = tmp_path / "session.img"
    image.write_bytes(bytes(1024))
    system = MegapadSystem(ram_size=64 * 1024, storage_image=str(image))

    with MachineSession(system):
        system.storage.write_sectors(0, 1, b"A" * 512)

    assert image.read_bytes()[:512] == b"A" * 512


def test_machine_session_close_releases_devices_when_save_fails(monkeypatch):
    backend = LoopbackBackend()
    session = MachineSession.from_bios(BIOS, nic_backend=backend)

    def fail_save():
        raise OSError("storage unavailable")

    monkeypatch.setattr(session.system.storage, "save_image", fail_save)
    try:
        session.close()
    except OSError as exc:
        assert str(exc) == "storage unavailable"
    else:
        raise AssertionError("storage failure should remain visible")

    assert session._closed
    assert not backend.link_up


def test_session_server_rejects_unavailable_tap(monkeypatch):
    monkeypatch.setattr(
        "sys.argv", ["session_server.py", "--nic-tap", "missing-tap"]
    )
    with patch("nic_backends.tap_available", return_value=False):
        try:
            session_server_main()
        except SystemExit as exc:
            assert exc.code == 2
        else:
            raise AssertionError("unavailable TAP should stop session startup")


def test_cli_uses_128_mib_external_memory_by_default(monkeypatch):
    monkeypatch.setattr("sys.argv", ["cli.py"])
    with (
        patch("cli.MegapadSystem", wraps=MegapadSystem) as system_factory,
        patch.object(MegapadCLI, "cmdloop", return_value=None),
    ):
        cli_main()

    assert system_factory.call_args.kwargs["ext_mem_size"] == 128 << 20
    assert system_factory.call_args.kwargs["worker_count"] is None


def test_cli_propagates_explicit_execution_lanes(monkeypatch):
    monkeypatch.setattr(
        "sys.argv",
        ["cli.py", "--cores", "3", "--lanes", "2"],
    )
    with (
        patch("cli.MegapadSystem", wraps=MegapadSystem) as system_factory,
        patch.object(MegapadCLI, "cmdloop", return_value=None),
    ):
        cli_main()

    assert system_factory.call_args.kwargs["worker_count"] == 2


def test_session_server_propagates_memory_and_lane_policy(monkeypatch):
    monkeypatch.setattr(
        "sys.argv",
        ["session_server.py", "--lanes", "4"],
    )
    with (
        patch("session_server.MachineSession.from_bios") as from_bios,
        patch("session_server.SharedMachine"),
        patch("session_server.SessionServer"),
        patch("session_server.signal.signal"),
    ):
        assert session_server_main() == 0

    assert from_bios.call_args.kwargs["ext_mem_size"] == 128 << 20
    assert from_bios.call_args.kwargs["lanes"] == 4
    assert from_bios.call_args.kwargs["presentation"] is None


def test_presentation_policy_derives_full_maximum_geometry_contract():
    policy = _presentation_policy()
    config = policy.configuration(100, 32)

    assert policy.maximum_transaction_bytes == 650_576
    assert policy.retained_publication_bytes == 654_672
    assert config.terminal_config.max_payload == 3_212
    assert config.terminal_config.max_transaction_bytes == 650_576
    assert config.terminal_config.terminal_receive_credit == 650_576
    assert config.terminal_config.max_cells == 80_000
    assert config.terminal_config.max_feed_bytes == 654_672
    assert config.terminal_config.cols == 100
    assert config.terminal_config.rows == 32
    assert config.host_limits.egress.high_bytes == 1_309_344
    assert config.host_limits.egress.low_bytes == 654_672
    assert config.host_limits.retained_publication_bytes == 654_672
    assert config.driver_limits.pending_outbound_events == 256


def test_presentation_policy_configuration_attaches_real_host_port():
    policy = _presentation_policy()
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=100,
        terminal_rows=32,
    )
    with MachineSession(
        system,
        cols=100,
        rows=32,
        presentation=policy.configuration(100, 32),
    ) as session:
        assert session.presentation_enabled
        assert session.presentation_state is TerminalState.ANSI
        assert system.presentation_terminal_host.enhanced_attached


def test_session_server_opt_in_uses_exact_presentation_policy(monkeypatch):
    policy = _presentation_policy()
    monkeypatch.setattr(
        "sys.argv",
        [
            "session_server.py",
            "--cols",
            "100",
            "--rows",
            "32",
            "--presentation-terminal-policy",
            json.dumps(policy.to_dict()),
        ],
    )
    with (
        patch("session_server.MachineSession.from_bios") as from_bios,
        patch("session_server.SharedMachine"),
        patch("session_server.SessionServer"),
        patch("session_server.signal.signal"),
    ):
        assert session_server_main() == 0

    config = from_bios.call_args.kwargs["presentation"]
    assert config == policy.configuration(100, 32)


def test_machine_session_warm_reset_discards_interrupted_uart_batch():
    with MachineSession.from_bios(BIOS, cols=80, rows=30) as session:
        session.boot()
        assert session.wait_for_idle(max_steps=2_000_000).reason == "idle"

        ring = session.system.uart._tx_ring_base
        assert ring
        session.system.cpu.mem[ring:ring + 8] = (4097).to_bytes(8, "little")

        session.reset()
        reboot = session.wait_for_idle(max_steps=2_000_000)

        assert reboot.reason == "idle"
        assert "Megapad-64 Forth BIOS" in session.raw_text()
        assert int.from_bytes(session.system.cpu.mem[ring:ring + 8], "little") == 0


def test_json_scenario_runner(tmp_path):
    scenario = tmp_path / "smoke.json"
    report = tmp_path / "report.json"
    image = tmp_path / "screen.png"
    scenario.write_text(json.dumps({
        "name": "pytest-smoke",
        "machine": {
            "bios": str(BIOS),
            "cols": 60,
            "rows": 20,
            "lanes": 4,
        },
        "actions": [
            {"type": "wait_idle", "max_steps": 2_000_000},
            {"type": "send_text", "text": "40 2 + .\n"},
            {
                "type": "wait_text",
                "text": "42 ",
                "scope": "raw",
                "max_steps": 2_000_000,
            },
            {"type": "capture", "png": str(image)},
        ],
        "report": str(report),
    }), encoding="utf-8")

    with patch(
        "dev_session.MachineSession.from_bios",
        wraps=MachineSession.from_bios,
    ) as from_bios:
        summary = run_scenario(scenario)

    assert summary["success"]
    assert from_bios.call_args.kwargs["ext_mem_size"] == 128 << 20
    assert from_bios.call_args.kwargs["lanes"] == 4
    assert summary["machine"]["lanes"] == 4
    assert summary["uart"]["byte_callbacks"] == 0
    assert image.is_file()
    assert json.loads(report.read_text())["success"]
