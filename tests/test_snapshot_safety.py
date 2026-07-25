"""Safety oracles for timeline snapshots and warm frontend resets."""

from __future__ import annotations

import json
import struct
import zlib

import pytest

from cli import MegapadCLI
from display import load_snapshot, save_snapshot
from session import MachineSession
from system import MegapadSystem


def _event_signature(events) -> tuple:
    return tuple(
        (
            int(event.cycle),
            int(event.sequence),
            str(event.kind),
            bytes(event.payload),
            int(event.argument0),
            int(event.argument1),
        )
        for event in events
    )


def _bus_signature(system: MegapadSystem) -> tuple:
    snapshot = system._native_system._main_bus_snapshot()
    grant = snapshot.active_grant
    active = None
    if grant is not None:
        active = (
            int(grant.grant_sequence),
            int(grant.grant_cycle),
            int(grant.timeout_cycle),
        )
    return (
        int(snapshot.schema_version),
        int(snapshot.port_count),
        int(snapshot.last_grant),
        bool(snapshot.reset_port_zero_credit),
        int(snapshot.next_grant_sequence),
        int(snapshot.earliest_arbitration_cycle),
        bool(snapshot.served_last),
        snapshot.last_arbitration_cycle,
        active,
        tuple(int(value) for value in snapshot.last_issue_sequences),
        tuple(int(value) for value in snapshot.sticky_bus_errors),
    )


def _machine_signature(system: MegapadSystem) -> tuple:
    core = system.cpu
    native = system._native_system
    return (
        bytes(system._shared_mem),
        bytes(system._hbw_mem),
        bytes(system._ext_mem),
        bytes(system._vram_mem),
        tuple(int(value) for value in core.regs),
        int(core.pc),
        int(core.cycle_count),
        bool(core.halted),
        bool(core.idle),
        (
            int(system.timer.counter),
            int(system.timer.compare),
            int(system.timer.control),
            int(system.timer.status),
            bool(system.timer.irq_pending),
        ),
        tuple(system.fb.snapshot()),
        tuple(system.rtc.snapshot()),
        (
            int(system.uart_geom.cols),
            int(system.uart_geom.rows),
            int(system.uart_geom.status),
            int(system.uart_geom.ctrl),
        ),
        int(system.uart.rx_pending),
        tuple(native.system_clock_snapshot()),
        _event_signature(native.external_event_pending),
        _event_signature(native.external_event_history),
        int(native.external_event_next_sequence),
        _bus_signature(system),
        bool(native.cycle_execution_pending),
        bool(system._booted),
    )


def _write_legacy_v1_snapshot(path, ram_size: int) -> None:
    metadata = json.dumps(
        {
            "version": 1,
            "ram_size": ram_size,
            "hbw_size": 0,
            "ext_mem_size": 0,
            "num_cores": 1,
            "num_clusters": 0,
            "cores": [],
        }
    ).encode("utf-8")
    ram = zlib.compress(bytes([0xEE]) * ram_size)
    path.write_bytes(
        b"MP64SNAP"
        + struct.pack("<IIII", len(metadata), len(ram), 0, 0)
        + metadata
        + ram
    )


def test_phase2_snapshot_save_rejects_before_touching_destination(tmp_path):
    system = MegapadSystem(ram_size=4096)
    destination = tmp_path / "existing.mp64"
    destination.write_bytes(b"keep this file intact")

    with pytest.raises(RuntimeError, match="native Phase 2 timeline"):
        save_snapshot(system, str(destination))

    assert destination.read_bytes() == b"keep this file intact"


def test_phase2_snapshot_load_rejects_before_mutating_machine(
    tmp_path,
    capsys,
):
    system = MegapadSystem(
        ram_size=4096,
        hbw_size=64,
        ext_mem_size=64,
        vram_size=64,
    )
    system._shared_mem[:4] = b"RAM!"
    system._hbw_mem[:4] = b"HBW!"
    system._ext_mem[:4] = b"EXT!"
    system._vram_mem[:4] = b"VRAM"
    system.cpu.regs[3] = 0x1234
    system.advance_system_cycles(7)
    system.schedule_uart_input(b"Q")
    system.schedule_nic_frame(b"\x01\x02\x03", at_cycle=11)
    system._booted = True
    before = _machine_signature(system)

    legacy = tmp_path / "legacy-v1.mp64"
    _write_legacy_v1_snapshot(legacy, system.ram_size)

    assert not load_snapshot(system, str(legacy))
    assert _machine_signature(system) == before
    assert "Restore unavailable" in capsys.readouterr().out


def test_warm_session_reset_preserves_ingress_and_discards_only_output():
    system = MegapadSystem(ram_size=4096)
    with MachineSession(
        system,
        cols=80,
        rows=24,
        batch_steps=16,
    ) as session:
        system.schedule_uart_input(b"A")
        system.schedule_nic_frame(b"\x01\x02", at_cycle=5)
        system.cpu._cs.uart_write8(0x00, ord("Z"))
        before_pending = _event_signature(
            system._native_system.external_event_pending
        )
        before_history = _event_signature(
            system._native_system.external_event_history
        )
        before_sequence = (
            system._native_system.external_event_next_sequence
        )

        session.reset(clear_terminal=False)

        assert system.cpu._cs.uart_read8(0x01) == ord("A")
        assert system.cpu._cs.uart_drain_tx() == b""
        assert system.uart._tx_ring_base == 0
        assert _event_signature(
            system._native_system.external_event_pending
        ) == before_pending
        assert _event_signature(
            system._native_system.external_event_history
        ) == before_history
        assert (
            system._native_system.external_event_next_sequence
            == before_sequence
        )


def test_cli_nic_reset_cannot_erase_unjournaled_device_state(capsys):
    system = MegapadSystem(ram_size=4096)
    system.schedule_nic_frame(b"\x01\x02\x03")
    cli = MegapadCLI(system)
    before_history = _event_signature(
        system._native_system.external_event_history
    )

    cli.do_nic("reset")

    assert system.cpu._cs.nic_rx_queue_size() == 1
    assert _event_signature(
        system._native_system.external_event_history
    ) == before_history
    assert not hasattr(system._native_system, "_reset_external_events")
    assert "NIC reset unavailable" in capsys.readouterr().out
