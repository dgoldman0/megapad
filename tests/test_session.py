"""Focused tests for the native UART and headless development session."""

from __future__ import annotations

import json
from pathlib import Path

from dev_session import run_scenario
from devices import UART
from display import VirtualTerminal
from session import MachineSession
from system import EXT_MEM_BASE, HBW_BASE, VRAM_BASE, MegapadSystem


ROOT = Path(__file__).resolve().parents[1]
BIOS = ROOT / "bios.asm"


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
    assert not system.cores[1]._cs.uart_enabled()
    system.uart.inject_input(b"X")
    # A secondary core's fallback callback reaches the shared core-0 queue.
    assert system.cores[1]._mmio_read8(0xFFFF_FF00_0000_0001) == ord("X")


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

    summary = run_scenario(scenario)

    assert summary["success"]
    assert summary["uart"]["byte_callbacks"] == 0
    assert image.is_file()
    assert json.loads(report.read_text())["success"]
