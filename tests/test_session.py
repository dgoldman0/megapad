"""Focused tests for the native UART and headless development session."""

from __future__ import annotations

import json
from dataclasses import replace
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

import pytest

from cli import MegapadCLI, main as cli_main
from dev_session import run_scenario
from devices import UART
from display import VirtualTerminal
from nic_backends import LoopbackBackend
from rich_terminal import (
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
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_view import DisplayScope, RetainedRootLabelPlane
from rich_terminal.update_authority import TerminalGeometry, TerminalUpdateError
from rich_terminal.retained_model import RetainedFeature, RetainedPolicy
from session import (
    MachineSession,
    RichTerminalSessionConfig,
    RichTerminalSessionPolicy,
    TerminalDisplayOffer,
)
from session_server import main as session_server_main
from system import EXT_MEM_BASE, HBW_BASE, VRAM_BASE, MegapadSystem


ROOT = Path(__file__).resolve().parents[1]
BIOS = ROOT / "bios.asm"


def _rich_terminal_config(
    *,
    ansi_history_bytes: int = 32,
    retained_policy: RetainedPolicy | None = None,
) -> RichTerminalSessionConfig:
    return RichTerminalSessionConfig(
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
            max_cols=4,
            max_rows=4,
            cols=2,
            rows=2,
        ),
        driver_limits=DriverLimits(4_096, 8),
        ansi_history_bytes=ansi_history_bytes,
        service_batches=2,
        retained_policy=retained_policy,
    )


def _retained_policy(*, interval_us: int = 0) -> RetainedPolicy:
    return RetainedPolicy(
        features=(
            RetainedFeature.CORE
            if interval_us == 0
            else RetainedFeature.CORE | RetainedFeature.CADENCE
        ),
        max_owner_records=1,
        max_live_owners=1,
        max_regions=1,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=1,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=512,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_label_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=interval_us,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=256,
        terminal_to_client_max_payload=256,
        base_max_transaction_bytes=512,
    )


def _rich_terminal_policy() -> RichTerminalSessionPolicy:
    return RichTerminalSessionPolicy(
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
        rich_terminal=_rich_terminal_config(),
    ) as session:
        assert session.rich_terminal_enabled
        assert session.rich_terminal_state is TerminalState.ANSI
        assert system.rich_terminal_host.enhanced_attached
        assert system.rich_terminal_host.pending_geometry_events == 1
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
        rich_terminal=_rich_terminal_config(ansi_history_bytes=4),
    ) as session:
        payload = b"\x1b[31mA"
        for value in payload:
            system.cpu._cs.uart_write8(0x00, value)
        assert system._drain_native_uart_output() == payload
        serviced = session.service_rich_terminal()
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
        rich_terminal=_rich_terminal_config(),
    ) as session:
        session.boot()
        first = session.rich_terminal_driver
        first_epoch = first.attachment_epoch
        first.close()
        system.cpu.halted = True
        assert session.run(max_steps=1).reason == "terminal_failure"
        assert "became stale" in session.rich_terminal_failure
        assert session.rich_terminal_lost
        assert session.rich_terminal_state is TerminalState.FAILED
        assert session.send_text("blocked") is DriverStatus.FAILED
        assert session.send_key("enter") is DriverStatus.FAILED
        assert session.resize(4, 1) is DriverStatus.FAILED
        session.reset()
        second = session.rich_terminal_driver
        assert second is not None
        assert second.attachment_epoch > first_epoch
        assert not session.rich_terminal_lost
        assert session.rich_terminal_state is TerminalState.ANSI
        assert system.rich_terminal_host.pending_geometry_events == 1


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
        rich_terminal=_rich_terminal_config(),
    ) as session:
        session.boot()

        def fail_boot(*args, **kwargs):
            raise RuntimeError("injected boot failure")

        monkeypatch.setattr(system, "boot", fail_boot)
        with pytest.raises(RuntimeError, match="injected boot failure"):
            session.boot()

        assert session.rich_terminal_driver is None
        assert session.rich_terminal_lost
        assert "rich-terminal boot failed" in session.rich_terminal_failure
        assert session.send_text("must not become raw") is DriverStatus.FAILED
        assert session.send_key("enter") is DriverStatus.FAILED
        assert session.resize(4, 1) is DriverStatus.FAILED
        assert session.run(max_steps=1).reason == "terminal_failure"
        with pytest.raises(TerminalSessionError, match="rich-terminal boot failed"):
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
            rich_terminal=_rich_terminal_config(),
        )

    assert system.uart.on_tx is byte_callback
    assert system.uart.on_tx_batch is batch_callback
    assert not system.rich_terminal_host.enhanced_attached


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
        rich_terminal=_rich_terminal_config(),
    ) as session:
        system.cpu.halted = True
        session.run_batch_stats(1)  # Cross the initial geometry boundary.
        view = TerminalView(
            attachment_epoch=session.rich_terminal_driver.attachment_epoch,
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
        session._receive_terminal_output(view)
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
        session.rich_terminal_driver.core.select_ansi_geometry(4, 1)
        session._sync_rich_terminal_geometry()
        assert (session.terminal.cols, session.terminal.rows) == (4, 1)
        assert session.snapshot().lines() == ["AB", "CD"]
        assert session.revision == before_sync
        session._refresh_output_display_boundary()
        assert (session.snapshot().cols, session.snapshot().rows) == (4, 1)
        assert session.revision == before_sync + 1


def test_machine_session_coalesces_logical_composites_at_owner_boundaries():
    policy = _retained_policy(interval_us=100)
    product = _rich_terminal_policy()
    assert product.configuration(2, 2, retained_policy=policy).retained_policy is policy
    assert "retained_policy" not in product.to_dict()

    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(retained_policy=policy),
    ) as session:
        driver = session.rich_terminal_driver
        assert driver is not None and driver.core.retained_configured
        system.cpu.halted = True
        session.run_batch_stats(1)  # Cross the initial host-geometry boundary.
        system.cpu.halted = False
        now = [1_000]
        assert session._display_cadence is not None
        session._display_cadence._monotonic_us = lambda: now[0]

        cell_one = TerminalView(
            attachment_epoch=driver.attachment_epoch,
            session_id=7,
            presentation_epoch=0,
            revision=1,
            cols=2,
            rows=2,
            cells=(
                (Cell(ord("A"), 7, 0), Cell(ord("A"), 7, 0)),
                (Cell(ord("A"), 7, 0), Cell(ord("A"), 7, 0)),
            ),
            dirty_spans=(),
            cursor=Cursor(0, 0, True),
        )
        geometry = TerminalGeometry(2, 2)
        first = CompositeTerminalView(0, 1, geometry, cell_one, None)
        core = driver.core
        core._retained_enabled = True
        core._coordinator = SimpleNamespace(view=first)
        core._clock = SimpleNamespace(revision=1)
        core._state = TerminalState.ACTIVE

        session._receive_terminal_output(cell_one)
        session._receive_terminal_output(first)
        assert session.logical_output_view is first
        assert session.displayed_output_view is None
        before_offer_revision = session.revision
        assert session._service_display_cadence()
        offer = session.display_offer
        assert isinstance(offer, TerminalDisplayOffer)
        assert not hasattr(offer, "composite")
        assert session._display_offer_composite is first
        assert isinstance(offer.scope, DisplayScope)
        assert offer.scope.model_revision == 1
        assert offer.cell.lines() == ["AA", "AA"]
        assert isinstance(offer.retained, RetainedRootLabelPlane)
        assert not offer.retained.retained_visible
        assert session.displayed_output_view is None
        assert session.displayed_model_revision is None
        assert session.revision == before_offer_revision
        assert not session._display_cadence_has_pending_work()
        assert session.send_text("held before ACK") is DriverStatus.BACKPRESSURED

        assert session.acknowledge_display_offer(offer.offer_id, offer.scope)
        assert session.display_offer is None
        assert session.displayed_output_view is first
        assert session.displayed_model_revision == 1
        assert session.snapshot().lines() == ["AA", "AA"]

        cell_two = replace(
            cell_one,
            revision=2,
            cells=(
                (Cell(ord("B"), 7, 0), Cell(ord("B"), 7, 0)),
                (Cell(ord("B"), 7, 0), Cell(ord("B"), 7, 0)),
            ),
        )
        second = CompositeTerminalView(0, 2, geometry, cell_two, None)
        core._coordinator.view = second
        core._clock.revision = 2
        session._receive_terminal_output(second)
        now[0] = 1_050
        assert not session._service_display_cadence()
        assert session.logical_output_view is second
        assert session.displayed_output_view is first
        assert session.snapshot().lines() == ["AA", "AA"]
        assert session.rich_terminal_work_pending
        assert session.send_text("held") is DriverStatus.BACKPRESSURED

        # A tombstone-only lifecycle publication may advance the composite
        # while structurally sharing every plane.  Cadence follows the global
        # revision rather than CELL object identity.
        latest = replace(second, revision=3)
        core._coordinator.view = latest
        core._clock.revision = 3
        session._receive_terminal_output(latest)
        cadence_reads = iter((1_050, 1_100, 1_100))
        session._display_cadence._monotonic_us = lambda: next(cadence_reads)
        guest_batches = []

        def forbidden_guest_batch(count):
            guest_batches.append(count)
            raise AssertionError("cadence-only wait ran a guest batch")

        system.run_batch_stats = forbidden_guest_batch
        system.cpu.halted = True
        halted = session.run(max_steps=1, wall_timeout_s=0.1)

        assert halted.reason == "halted"
        assert halted.steps == 0 and halted.batches == 0
        assert guest_batches == []
        latest_offer = session.display_offer
        assert latest_offer is not None
        assert session._display_offer_composite is latest
        assert session.displayed_output_view is first
        assert session.displayed_model_revision == 1
        assert not session._display_cadence_has_pending_work()
        assert session.acknowledge_display_offer(
            latest_offer.offer_id,
            latest_offer.scope,
        )
        assert session.displayed_output_view is latest
        assert session.displayed_model_revision == 3
        assert session.snapshot().lines() == ["BB", "BB"]

        cell_three = replace(
            cell_two,
            revision=4,
            cells=(
                (Cell(ord("C"), 7, 0), Cell(ord("C"), 7, 0)),
                (Cell(ord("C"), 7, 0), Cell(ord("C"), 7, 0)),
            ),
        )
        idle_view = CompositeTerminalView(0, 4, geometry, cell_three, None)
        core._coordinator.view = idle_view
        core._clock.revision = 4
        session._receive_terminal_output(idle_view)
        cadence_reads = iter((1_150, 1_200, 1_200))
        session._display_cadence._monotonic_us = lambda: next(cadence_reads)
        system.cpu.halted = False
        system.cpu.idle = True
        idle = session.run(max_steps=1, wall_timeout_s=0.1)

        assert idle.reason == "idle"
        assert idle.steps == 0 and idle.batches == 0
        assert guest_batches == []
        idle_offer = session.display_offer
        assert idle_offer is not None
        assert session._display_offer_composite is idle_view
        assert session.displayed_output_view is latest
        assert session.acknowledge_display_offer(idle_offer.offer_id, idle_offer.scope)
        assert session.displayed_output_view is idle_view
        assert session.snapshot().lines() == ["CC", "CC"]


def test_machine_session_keeps_last_rich_view_until_a_valid_replacement():
    policy = _retained_policy()
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(retained_policy=policy),
    ) as session:
        driver = session.rich_terminal_driver
        assert driver is not None
        cell = TerminalView(
            attachment_epoch=driver.attachment_epoch,
            session_id=9,
            presentation_epoch=0,
            revision=1,
            cols=2,
            rows=2,
            cells=(
                (Cell(ord("R"), 7, 0), Cell(ord("R"), 7, 0)),
                (Cell(ord("R"), 7, 0), Cell(ord("R"), 7, 0)),
            ),
            dirty_spans=(),
            cursor=Cursor(0, 0, True),
        )
        rich = CompositeTerminalView(
            0,
            1,
            TerminalGeometry(2, 2),
            cell,
            None,
        )
        core = driver.core
        core._retained_enabled = True
        core._coordinator = SimpleNamespace(view=rich)
        core._clock = SimpleNamespace(revision=1)
        session._receive_terminal_output(cell)
        session._receive_terminal_output(rich)
        assert session._service_display_cadence()
        offer = session.display_offer
        assert offer is not None
        assert session.displayed_output_view is None
        assert session.acknowledge_display_offer(offer.offer_id, offer.scope)
        acknowledged_revision = session.revision
        assert not session.acknowledge_display_offer(offer.offer_id, offer.scope)
        assert session.revision == acknowledged_revision
        foreign_scope = replace(offer.scope, session_id=offer.scope.session_id + 1)
        with pytest.raises(TerminalUpdateError, match="stale or outside"):
            session.acknowledge_display_offer(offer.offer_id, foreign_scope)

        pending_cell = replace(cell, revision=2)
        pending_rich = CompositeTerminalView(
            0,
            2,
            TerminalGeometry(2, 2),
            pending_cell,
            None,
        )
        core._coordinator.view = pending_rich
        core._clock.revision = 2
        session._receive_terminal_output(pending_rich)
        assert session._service_display_cadence()
        pending_offer = session.display_offer
        assert pending_offer is not None

        core._retained_enabled = False
        core._coordinator = None
        assert not session._service_display_cadence()
        assert session.displayed_output_view is rich
        assert session.snapshot().lines() == ["RR", "RR"]

        replacement = replace(
            cell,
            presentation_epoch=1,
            revision=1,
            cells=(
                (Cell(ord("N"), 7, 0), Cell(ord("N"), 7, 0)),
                (Cell(ord("N"), 7, 0), Cell(ord("N"), 7, 0)),
            ),
        )
        session._receive_terminal_output(replacement)
        assert session.display_offer is None
        assert session.logical_output_view is None
        assert session.displayed_output_view is None
        assert session.snapshot().lines() == ["NN", "NN"]
        with pytest.raises(TerminalUpdateError, match="stale or outside"):
            session.acknowledge_display_offer(offer.offer_id, offer.scope)
        with pytest.raises(TerminalUpdateError, match="stale or outside"):
            session.acknowledge_display_offer(
                pending_offer.offer_id,
                pending_offer.scope,
            )

        assert session._display_cadence is not None
        replacement_rich = CompositeTerminalView(
            1,
            1,
            TerminalGeometry(2, 2),
            replacement,
            None,
        )
        core._retained_enabled = True
        core._coordinator = SimpleNamespace(view=replacement_rich)
        core._clock = SimpleNamespace(revision=1)
        session._receive_terminal_output(replacement_rich)
        assert session._service_display_cadence()
        replacement_offer = session.display_offer
        assert replacement_offer is not None
        assert replacement_offer.offer_id > pending_offer.offer_id
        assert session.acknowledge_display_offer(
            replacement_offer.offer_id,
            replacement_offer.scope,
        )
        assert session.displayed_output_view is replacement_rich


def test_machine_session_offer_does_not_promote_geometry_or_revision_before_ack():
    policy = _retained_policy()
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(retained_policy=policy),
    ) as session:
        driver = session.rich_terminal_driver
        assert driver is not None
        cell = TerminalView(
            attachment_epoch=driver.attachment_epoch,
            session_id=13,
            presentation_epoch=0,
            revision=1,
            cols=3,
            rows=1,
            cells=(
                (
                    Cell(ord("X"), 7, 0),
                    Cell(ord("Y"), 7, 0),
                    Cell(ord("Z"), 7, 0),
                ),
            ),
            dirty_spans=(),
            cursor=Cursor(2, 0, True),
        )
        composite = CompositeTerminalView(
            0,
            1,
            TerminalGeometry(3, 1),
            cell,
            None,
        )
        core = driver.core
        core._retained_enabled = True
        core._coordinator = SimpleNamespace(view=composite)
        core._clock = SimpleNamespace(revision=1)
        core._state = TerminalState.ACTIVE
        session._receive_terminal_output(composite)

        before_offer_revision = session.revision
        assert session.visible_geometry == (2, 2)
        assert session._service_display_cadence()
        offer = session.display_offer
        assert offer is not None
        assert offer.cell.lines() == ["XYZ"]
        assert session.visible_geometry == (2, 2)
        assert (session.terminal.cols, session.terminal.rows) == (2, 2)
        assert session.revision == before_offer_revision

        assert session.acknowledge_display_offer(offer.offer_id, offer.scope)
        assert session.visible_geometry == (3, 1)
        assert (session.terminal.cols, session.terminal.rows) == (3, 1)
        assert session.revision == before_offer_revision + 1
        assert session.snapshot().lines() == ["XYZ"]


def test_machine_session_revokes_and_reoffers_only_the_exact_display_candidate():
    policy = _retained_policy()
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(retained_policy=policy),
    ) as session:
        driver = session.rich_terminal_driver
        assert driver is not None
        cell = TerminalView(
            attachment_epoch=driver.attachment_epoch,
            session_id=11,
            presentation_epoch=0,
            revision=1,
            cols=2,
            rows=2,
            cells=(
                (Cell(ord("A"), 7, 0), Cell(ord("A"), 7, 0)),
                (Cell(ord("A"), 7, 0), Cell(ord("A"), 7, 0)),
            ),
            dirty_spans=(),
            cursor=Cursor(0, 0, True),
        )
        geometry = TerminalGeometry(2, 2)
        first = CompositeTerminalView(0, 1, geometry, cell, None)
        core = driver.core
        core._retained_enabled = True
        core._coordinator = SimpleNamespace(view=first)
        core._clock = SimpleNamespace(revision=1)
        core._state = TerminalState.ACTIVE
        session._receive_terminal_output(cell)
        session._receive_terminal_output(first)

        assert session._service_display_cadence()
        first_offer = session.display_offer
        assert first_offer is not None
        foreign_scope = replace(
            first_offer.scope,
            presentation_epoch=first_offer.scope.presentation_epoch + 1,
        )
        with pytest.raises(TerminalUpdateError, match="stale or outside"):
            session.revoke_display_offer(first_offer.offer_id, foreign_scope)
        with pytest.raises(TerminalUpdateError, match="stale or outside"):
            session.revoke_display_offer(first_offer.offer_id + 1, first_offer.scope)
        assert session.display_offer is first_offer

        before_revoke_revision = session.revision
        assert session.revoke_display_offer(first_offer.offer_id, first_offer.scope)
        assert session.display_offer is None
        assert session.displayed_output_view is None
        assert session.revision == before_revoke_revision
        assert session._display_cadence_has_pending_work()
        assert session.rich_terminal_work_pending
        with pytest.raises(TerminalUpdateError, match="stale or outside"):
            session.acknowledge_display_offer(
                first_offer.offer_id,
                first_offer.scope,
            )

        assert session._service_display_cadence()
        reoffer = session.display_offer
        assert reoffer is not None
        assert reoffer.offer_id > first_offer.offer_id
        assert session._display_offer_composite is first

        second_cell = replace(
            cell,
            revision=2,
            cells=(
                (Cell(ord("B"), 7, 0), Cell(ord("B"), 7, 0)),
                (Cell(ord("B"), 7, 0), Cell(ord("B"), 7, 0)),
            ),
        )
        second = CompositeTerminalView(0, 2, geometry, second_cell, None)
        core._coordinator.view = second
        core._clock.revision = 2
        session._receive_terminal_output(second)
        assert session.revoke_display_offer(reoffer.offer_id, reoffer.scope)
        assert session._display_cadence_has_pending_work()
        assert session.rich_terminal_work_pending
        assert session._service_display_cadence()
        latest_offer = session.display_offer
        assert latest_offer is not None
        assert session._display_offer_composite is second
        assert latest_offer.offer_id > reoffer.offer_id
        assert session.acknowledge_display_offer(
            latest_offer.offer_id,
            latest_offer.scope,
        )
        assert session.displayed_output_view is second
        assert session.snapshot().lines() == ["BB", "BB"]

        fallback_cell = replace(
            second_cell,
            revision=3,
            cells=(
                (Cell(ord("C"), 7, 0), Cell(ord("C"), 7, 0)),
                (Cell(ord("C"), 7, 0), Cell(ord("C"), 7, 0)),
            ),
        )
        before_fallback = CompositeTerminalView(0, 3, geometry, fallback_cell, None)
        core._coordinator.view = before_fallback
        core._clock.revision = 3
        session._receive_terminal_output(before_fallback)
        assert session._service_display_cadence()
        abandoned_offer = session.display_offer
        assert abandoned_offer is not None

        core._retained_enabled = False
        core._coordinator = None
        session._receive_terminal_output(fallback_cell)
        assert session.display_offer is None
        assert session._display_offer_composite is None
        assert session._display_cadence is None
        assert session.logical_output_view is None
        assert session.displayed_output_view is None
        assert session.snapshot().lines() == ["CC", "CC"]
        with pytest.raises(TerminalUpdateError, match="stale or outside"):
            session.acknowledge_display_offer(
                abandoned_offer.offer_id,
                abandoned_offer.scope,
            )


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
    assert from_bios.call_args.kwargs["rich_terminal"] is None


def test_rich_terminal_policy_derives_full_maximum_geometry_contract():
    policy = _rich_terminal_policy()
    config = policy.configuration(100, 32)

    assert policy.maximum_transaction_bytes == 650_576
    assert policy.retained_publication_bytes == 654_672
    assert config.terminal_config.max_payload == 3_212
    assert config.terminal_config.max_transaction_bytes == 650_576
    assert config.terminal_config.terminal_receive_credit == 650_576
    assert config.terminal_config.max_cells == 80_000
    assert config.terminal_config.max_feed_bytes == 654_672
    assert config.terminal_config.max_cols == 400
    assert config.terminal_config.max_rows == 200
    assert config.terminal_config.cols == 100
    assert config.terminal_config.rows == 32
    assert config.host_limits.egress.high_bytes == 1_309_344
    assert config.host_limits.egress.low_bytes == 654_672
    assert config.host_limits.retained_publication_bytes == 654_672
    assert config.driver_limits.pending_outbound_events == 256


def test_rich_terminal_policy_configuration_attaches_real_host_port():
    policy = _rich_terminal_policy()
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=100,
        terminal_rows=32,
    )
    with MachineSession(
        system,
        cols=100,
        rows=32,
        rich_terminal=policy.configuration(100, 32),
    ) as session:
        assert session.rich_terminal_enabled
        assert session.rich_terminal_state is TerminalState.ANSI
        assert system.rich_terminal_host.enhanced_attached
        assert session.resize(1, 201) is DriverStatus.INVALID
        assert session.rich_terminal_driver.core.selected_geometry == (100, 32)
        assert session.resize(400, 200) is DriverStatus.PROGRESS
        assert session.rich_terminal_driver.core.selected_geometry == (400, 200)


def test_session_server_opt_in_uses_exact_rich_terminal_policy(monkeypatch):
    policy = _rich_terminal_policy()
    monkeypatch.setattr(
        "sys.argv",
        [
            "session_server.py",
            "--cols",
            "100",
            "--rows",
            "32",
            "--rich-terminal-policy",
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

    config = from_bios.call_args.kwargs["rich_terminal"]
    assert config == policy.configuration(100, 32)


def test_session_server_carries_the_exact_retained_policy(monkeypatch):
    policy = _rich_terminal_policy()
    retained = RetainedPolicy(
        features=RetainedFeature.CORE | RetainedFeature.CADENCE,
        max_owner_records=1,
        max_live_owners=1,
        max_regions=1,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=1,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=policy.maximum_transaction_bytes,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_label_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=500_000,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=3_212,
        terminal_to_client_max_payload=3_212,
        base_max_transaction_bytes=policy.maximum_transaction_bytes,
    )
    monkeypatch.setattr(
        "sys.argv",
        [
            "session_server.py",
            "--cols",
            "100",
            "--rows",
            "32",
            "--rich-terminal-policy",
            json.dumps(policy.to_dict()),
            "--retained-terminal-policy",
            json.dumps(retained.to_dict()),
        ],
    )
    with (
        patch("session_server.MachineSession.from_bios") as from_bios,
        patch("session_server.SharedMachine"),
        patch("session_server.SessionServer"),
        patch("session_server.signal.signal"),
    ):
        assert session_server_main() == 0

    config = from_bios.call_args.kwargs["rich_terminal"]
    assert config == policy.configuration(100, 32, retained_policy=retained)
    assert config.retained_policy is not None
    assert config.retained_policy.to_dict() == retained.to_dict()


def test_session_server_rejects_retained_policy_without_base_attachment(monkeypatch):
    retained = _retained_policy()
    monkeypatch.setattr(
        "sys.argv",
        [
            "session_server.py",
            "--retained-terminal-policy",
            json.dumps(retained.to_dict()),
        ],
    )
    with patch("session_server.MachineSession.from_bios") as from_bios:
        with pytest.raises(SystemExit, match="2"):
            session_server_main()
    from_bios.assert_not_called()


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
