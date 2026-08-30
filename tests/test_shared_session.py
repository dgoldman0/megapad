"""Tests for the shared single-owner session protocol."""

from __future__ import annotations

import base64
import threading
import time
from dataclasses import replace
from pathlib import Path
from types import MappingProxyType, SimpleNamespace

import pytest

from rich_terminal import (
    Cell,
    Cursor,
    DriverLimits,
    DriverStatus,
    EgressWatermarks,
    HostPortLimits,
    TerminalConfig,
    TerminalState,
    TerminalView,
)
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import RetainedFeature, RetainedPolicy
from rich_terminal.retained_scene import (
    ObjectBounds,
    RGBA,
    RetainedScene,
    SceneModelState,
)
from rich_terminal.retained_view import (
    DisplayScope,
    GlyphRunDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
)
from rich_terminal.update_authority import TerminalGeometry
from session import (
    MachineSession,
    RichTerminalSessionConfig,
    TerminalCell,
    TerminalDisplayOffer,
    TerminalSnapshot,
)
from shared_session import (
    SessionClient,
    SessionServer,
    SharedMachine,
    display_offer_from_wire,
    display_offer_to_wire,
    display_scope_from_wire,
    display_scope_to_wire,
    retained_draw_plane_from_wire,
    retained_draw_plane_to_wire,
    snapshot_from_wire,
    snapshot_to_wire,
)
from system import MegapadSystem, SystemRunStats


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


def _retained_policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE,
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
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=256,
        terminal_to_client_max_payload=256,
        base_max_transaction_bytes=512,
    )


def _arm_retained_offer(
    session: MachineSession,
    *,
    char: str = "A",
    revision: int = 1,
) -> CompositeTerminalView:
    driver = session.rich_terminal_driver
    assert driver is not None
    cell = TerminalView(
        attachment_epoch=driver.attachment_epoch,
        session_id=7,
        presentation_epoch=0,
        revision=revision,
        cols=2,
        rows=2,
        cells=(
            (Cell(ord(char), 7, 0), Cell(ord(char), 7, 0)),
            (Cell(ord(char), 7, 0), Cell(ord(char), 7, 0)),
        ),
        dirty_spans=(),
        cursor=Cursor(0, 0, True),
    )
    composite = CompositeTerminalView(
        0,
        revision,
        TerminalGeometry(2, 2),
        cell,
        SceneModelState(
            revision=revision,
            geometry=TerminalGeometry(2, 2),
            active=RetainedScene(MappingProxyType({})),
            hidden=None,
            hidden_kind=None,
            requirement=None,
            retained_visible=True,
            retained_initialized=True,
        ),
    )
    core = driver.core
    core._retained_enabled = True
    core._coordinator = SimpleNamespace(view=composite)
    core._clock = SimpleNamespace(revision=revision)
    core._state = TerminalState.ACTIVE
    session._receive_terminal_output(composite)
    assert session._service_display_cadence()
    return composite


def wait_until(predicate, timeout=3.0):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        value = predicate()
        if value:
            return value
        time.sleep(0.01)
    raise AssertionError("condition did not become true")


class _RunLoopUART:
    has_rx_data = False


class _RunLoopSystem:
    def __init__(self):
        self.all_halted = True
        self.all_idle_or_halted = True
        self.uart = _RunLoopUART()


class _RunLoopSession:
    def __init__(self, *, remain_pending: bool):
        self.system = _RunLoopSystem()
        self.batch_steps = 17
        self.rich_terminal_enabled = True
        self.rich_terminal_failure = None
        self.rich_terminal_lost = False
        self.rich_terminal_work_pending = True
        self.last_batch_made_progress = False
        self.remain_pending = remain_pending
        self.calls = 0
        self.called = threading.Event()
        self.closed = False

    def boot(self):
        pass

    def close(self):
        self.closed = True

    def run_batch_stats(self, steps):
        assert steps == self.batch_steps
        self.calls += 1
        self.called.set()
        if not self.remain_pending:
            self.rich_terminal_work_pending = False
            self.last_batch_made_progress = True
            return SystemRunStats(
                instructions_executed=0,
                system_cycles_advanced=0,
                per_core_instructions=(0,),
                per_core_cycles=(0,),
                system_stop_reason="all_halted",
                external_events_applied=1,
            )
        self.last_batch_made_progress = False
        return SystemRunStats(
            instructions_executed=0,
            system_cycles_advanced=0,
            per_core_instructions=(0,),
            per_core_cycles=(0,),
            system_stop_reason="host_backpressure",
        )


def _phase_event(sequence: int, phase: int) -> int:
    return (sequence << 8) | phase


class _PhaseEventCPU:
    def __init__(self, value: int):
        self.value = value
        self.address = 0x100
        self.reads = 0
        self.read_error: Exception | None = None

    def mem_read64(self, address: int) -> int:
        assert address == self.address
        self.reads += 1
        if self.read_error is not None:
            raise self.read_error
        return self.value


class _PhaseEventSystem:
    def __init__(self, value: int):
        self.cpu = _PhaseEventCPU(value)
        self.ram_size = 0x1_000
        self.ext_mem_size = 0
        self.ext_mem_base = 0
        self.ext_mem_end = 0
        self.all_halted = False
        self.all_idle_or_halted = False
        self.uart = _RunLoopUART()


class _PhaseEventSession:
    def __init__(
        self,
        events: list[tuple[int, int]],
        *,
        initial_event: int = 0,
    ):
        self.system = _PhaseEventSystem(initial_event)
        self.events = list(events)
        self.batch_steps = 17
        self.rich_terminal_enabled = False
        self.rich_terminal_failure = None
        self.rich_terminal_lost = False
        self.rich_terminal_work_pending = False
        self.last_batch_made_progress = True
        self.closed = False
        self.resets = 0

    def boot(self):
        pass

    def close(self):
        self.closed = True

    def reset(self):
        self.resets += 1

    def run_batch_stats(self, steps):
        assert steps in {1, self.batch_steps}
        if not self.events:
            self.system.all_halted = True
            self.system.all_idle_or_halted = True
            self.last_batch_made_progress = False
            return SystemRunStats(
                instructions_executed=0,
                system_cycles_advanced=0,
                per_core_instructions=(0,),
                per_core_cycles=(0,),
                system_stop_reason="all_halted",
            )
        instructions, event = self.events.pop(0)
        self.system.cpu.value = event
        if not self.events:
            self.system.all_halted = True
            self.system.all_idle_or_halted = True
        return SystemRunStats(
            instructions_executed=instructions,
            system_cycles_advanced=instructions,
            per_core_instructions=(instructions,),
            per_core_cycles=(instructions,),
            system_stop_reason=(
                "all_halted" if self.system.all_halted else "instruction_limit"
            ),
        )


def _mark_phase_machine_running(
    machine: SharedMachine,
    *,
    generation: int = 1,
) -> None:
    machine._reset_generation = generation
    machine._thread = threading.current_thread()


def test_shared_owner_services_rich_terminal_work_after_guest_halt():
    session = _RunLoopSession(remain_pending=False)
    machine = SharedMachine(session, idle_sleep_s=0.005)

    machine.start()
    try:
        assert session.called.wait(timeout=1.0)
        wait_until(lambda: not session.rich_terminal_work_pending)
        assert session.calls == 1
        assert machine.total_steps == 0
        assert machine.last_stop_reason == "all_halted"
        assert machine.last_error is None
    finally:
        machine.stop()

    assert session.closed


def test_shared_owner_waits_on_zero_progress_host_backpressure():
    session = _RunLoopSession(remain_pending=True)
    machine = SharedMachine(session, idle_sleep_s=0.02)

    machine.start()
    try:
        assert session.called.wait(timeout=1.0)
        time.sleep(0.07)
        assert session.calls < 20
        assert machine.total_steps == 0
        assert machine.total_batches == 0
        assert machine.last_stop_reason == "host_backpressure"
        assert machine.last_error is None
        assert not machine.paused
    finally:
        machine.stop()


def test_phase_profile_disabled_run_performs_no_guest_reads():
    session = _PhaseEventSession([(17, _phase_event(1, 3))])
    machine = SharedMachine(session, idle_sleep_s=0.005)

    machine.start()
    try:
        wait_until(lambda: machine.total_steps == 17)
        assert session.system.cpu.reads == 0
        assert machine.phase_profile()["status"] == "disabled"
        assert session.system.cpu.reads == 0
    finally:
        machine.stop()


def test_phase_profile_run_loop_samples_after_exact_step_accounting():
    session = _PhaseEventSession([(17, _phase_event(3, 5))])
    machine = SharedMachine(session, idle_sleep_s=0.005)
    machine.paused = True

    machine.start()
    try:
        started = machine.start_phase_profile(0x100, 4, generation=1)
        assert started["machine_generation"] == 1
        with machine.condition:
            machine.paused = False
            machine.condition.notify_all()
        wait_until(lambda: machine.total_steps == 17)
        assert machine.phase_profile()["transitions"] == [
            {
                "machine_generation": 1,
                "sample_index": 1,
                "source": "run_batch",
                "batch_index": 1,
                "step_lower_bound": 0,
                "step_upper_bound": 17,
                "previous_event": _phase_event(0, 0),
                "previous_sequence": 0,
                "previous_phase": 0,
                "event": _phase_event(3, 5),
                "sequence": 3,
                "phase": 5,
                "coalesced_transitions": 2,
            }
        ]
    finally:
        machine.stop()


def test_phase_profile_records_exact_batch_bounds_and_bounded_coalescing():
    session = _PhaseEventSession([], initial_event=_phase_event(7, 0))
    machine = SharedMachine(session)
    _mark_phase_machine_running(machine, generation=3)
    machine.total_steps = 100
    machine.total_batches = 4
    server = SessionServer(machine, "unused.sock")

    started = server.dispatch(
        "start_phase_profile",
        {"generation": 3, "address": 0x100, "max_events": 1},
    )
    assert started["status"] == "active"
    assert started["machine_generation"] == 3
    assert started["initial"] == {
        "event": _phase_event(7, 0),
        "sequence": 7,
        "phase": 0,
    }

    session.system.cpu.value = _phase_event(10, 3)
    machine.total_steps = 117
    machine.total_batches = 5
    machine._sample_phase_profile(
        100,
        117,
        source="run_batch",
        batch_index=5,
    )
    session.system.cpu.value = _phase_event(11, 0)
    machine.total_steps = 134
    machine.total_batches = 6
    machine._sample_phase_profile(
        117,
        134,
        source="run_batch",
        batch_index=6,
    )

    reads_before_snapshot = session.system.cpu.reads
    snapshot = server.dispatch("phase_profile", {})
    assert session.system.cpu.reads == reads_before_snapshot
    assert snapshot["observed_transitions"] == 4
    assert snapshot["coalesced_transitions"] == 2
    assert snapshot["dropped_records"] == 1
    assert snapshot["dropped_transitions"] == 1
    assert snapshot["transitions"] == [
        {
            "machine_generation": 3,
            "sample_index": 1,
            "source": "run_batch",
            "batch_index": 5,
            "step_lower_bound": 100,
            "step_upper_bound": 117,
            "previous_event": _phase_event(7, 0),
            "previous_sequence": 7,
            "previous_phase": 0,
            "event": _phase_event(10, 3),
            "sequence": 10,
            "phase": 3,
            "coalesced_transitions": 2,
        }
    ]

    stopped = server.dispatch("stop_phase_profile", {})
    assert stopped["status"] == "stopped"
    assert stopped["stopped_steps"] == 134
    assert server.dispatch("phase_profile", {})["status"] == "disabled"


def test_phase_profile_accepts_unaligned_cell_but_rejects_unsafe_spans():
    session = _PhaseEventSession([])
    machine = SharedMachine(session)
    _mark_phase_machine_running(machine)

    session.system.cpu.address = 0x101
    started = machine.start_phase_profile(0x101, 4, generation=1)
    assert started["address"] == 0x101
    assert session.system.cpu.reads == 1
    machine.stop_phase_profile()

    with pytest.raises(ValueError, match="complete RAM"):
        machine.start_phase_profile(0xFF9, 4, generation=1)
    with pytest.raises(ValueError, match="complete RAM"):
        machine.start_phase_profile(0x1_000, 4, generation=1)
    with pytest.raises(ValueError, match="between 1 and 65536"):
        machine.start_phase_profile(0x100, 65_537, generation=1)

    assert session.system.cpu.reads == 1


def test_phase_profile_start_is_live_and_generation_bound():
    session = _PhaseEventSession([])
    machine = SharedMachine(session)
    server = SessionServer(machine, "unused.sock")

    with pytest.raises(RuntimeError, match="running machine"):
        machine.start_phase_profile(0x100, 4, generation=0)

    _mark_phase_machine_running(machine, generation=3)
    with pytest.raises(RuntimeError, match="stale phase profile generation"):
        machine.start_phase_profile(0x100, 4, generation=2)
    with pytest.raises(ValueError, match="fields are not exact"):
        server.dispatch(
            "start_phase_profile",
            {"address": 0x100, "max_events": 4},
        )

    machine._stopping = True
    with pytest.raises(RuntimeError, match="running machine"):
        machine.start_phase_profile(0x100, 4, generation=3)
    machine._stopping = False
    machine._thread = threading.Thread()
    with pytest.raises(RuntimeError, match="running machine"):
        machine.start_phase_profile(0x100, 4, generation=3)

    assert session.system.cpu.reads == 0


def test_phase_profile_read_failure_freezes_only_the_observer():
    session = _PhaseEventSession([], initial_event=_phase_event(1, 0))
    machine = SharedMachine(session)
    _mark_phase_machine_running(machine)
    machine.start_phase_profile(0x100, 4, generation=1)
    session.system.cpu.read_error = RuntimeError("diagnostic read failed")

    machine.total_steps = 17
    machine.total_batches = 1
    machine._sample_phase_profile(
        0,
        17,
        source="run_batch",
        batch_index=1,
    )
    reads_after_failure = session.system.cpu.reads
    machine.total_steps = 34
    machine.total_batches = 2
    machine._sample_phase_profile(
        17,
        34,
        source="run_batch",
        batch_index=2,
    )

    snapshot = machine.phase_profile()
    assert session.system.cpu.reads == reads_after_failure
    assert snapshot["status"] == "read_error"
    assert snapshot["last_sample_steps"] == 0
    assert snapshot["stopped_steps"] == 17
    assert snapshot["error"] == {
        "kind": "RuntimeError",
        "message": "diagnostic read failed",
    }
    assert machine.last_error is None
    assert not machine.paused

    stopped = machine.stop_phase_profile()
    assert stopped["status"] == "read_error"
    assert stopped["stopped_steps"] == 17


def test_phase_profile_reset_and_machine_stop_discard_observer_state():
    session = _PhaseEventSession([], initial_event=_phase_event(1, 0))
    machine = SharedMachine(session)
    _mark_phase_machine_running(machine)
    machine.status = lambda **_kwargs: {
        "phase_profile_configured": machine._phase_profile is not None
    }
    machine.start_phase_profile(0x100, 4, generation=1)

    reset = machine.reset(paused=True)
    assert reset == {"phase_profile_configured": False}
    assert session.resets == 1
    assert machine.phase_profile()["status"] == "disabled"

    machine.start_phase_profile(0x100, 4, generation=2)
    machine.stop()
    assert machine._phase_profile is None
    assert session.closed


def test_phase_profile_samples_each_explicit_step_after_accounting():
    session = _PhaseEventSession(
        [
            (1, _phase_event(1, 3)),
            (1, _phase_event(2, 0)),
        ]
    )
    machine = SharedMachine(session)
    machine.paused = True
    _mark_phase_machine_running(machine)
    machine.status = lambda **_kwargs: {"steps": machine.total_steps}
    machine.start_phase_profile(0x100, 4, generation=1)

    result = machine.step(2)

    assert result["executed"] == 2
    assert result["status"] == {"steps": 2}
    assert [
        (
            item["source"],
            item["batch_index"],
            item["step_lower_bound"],
            item["step_upper_bound"],
        )
        for item in machine.phase_profile()["transitions"]
    ] == [("step", None, 0, 1), ("step", None, 1, 2)]


def test_snapshot_wire_round_trip():
    with MachineSession.from_bios(BIOS, cols=40, rows=12) as session:
        session.boot()
        session.wait_for_idle(max_steps=2_000_000)
        original = session.snapshot()
        restored = snapshot_from_wire(snapshot_to_wire(original))
        assert restored == original


def test_snapshot_wire_preserves_an_invisible_cursor_outside_geometry():
    snapshot = TerminalSnapshot(
        1,
        1,
        ((TerminalCell(" ", (0, 0, 0), (0, 0, 0), 0),),),
        cursor_col=(1 << 32) - 1,
        cursor_row=7,
        cursor_visible=False,
        alternate_screen=False,
    )
    assert snapshot_from_wire(snapshot_to_wire(snapshot)) == snapshot

    visible = snapshot_to_wire(replace(snapshot, cursor_visible=True))
    with pytest.raises(ValueError, match="inside the geometry"):
        snapshot_from_wire(visible)


def test_display_offer_wire_round_trip_is_lossless_and_bounded():
    scope = DisplayScope(
        attachment_epoch=3,
        session_id=5,
        presentation_epoch=7,
        model_revision=11,
        geometry_generation=13,
        cell_revision=9,
        retained_revision=11,
    )
    draw = GlyphRunDraw(
        object_id=17,
        z_order=-2,
        bounds=ObjectBounds(1, 2, 3, 4),
        foreground=RGBA(5, 6, 7, 8),
        background=RGBA(9, 10, 11, 12),
        attributes=0x40,
        text="visible",
    )
    plane = RetainedDrawPlane(
        retained_initialized=True,
        retained_visible=True,
        regions=(
            RetainedRegionDraw(
                owner_id=19,
                owner_generation=23,
                region_id=29,
                cell_x=1,
                cell_y=2,
                cell_cols=3,
                cell_rows=4,
                z_order=-1,
                clipped=True,
                draws=(draw,),
            ),
        ),
    )
    snapshot = TerminalSnapshot(
        cols=1,
        rows=1,
        cells=((TerminalCell("X", (1, 2, 3), (4, 5, 6), 0x80),),),
        cursor_col=0,
        cursor_row=0,
        cursor_visible=True,
        alternate_screen=False,
    )
    offer = TerminalDisplayOffer((1 << 64) + 31, scope, snapshot, plane)

    assert display_scope_from_wire(display_scope_to_wire(scope)) == scope
    assert (
        retained_draw_plane_from_wire(
            retained_draw_plane_to_wire(plane)
        )
        == plane
    )
    wire = display_offer_to_wire(offer)
    assert display_offer_from_wire(wire) == offer
    assert set(wire) == {"offer_id", "scope", "cell", "retained"}
    wire_draw = wire["retained"]["regions"][0]["draws"][0]
    assert wire_draw == {
        "kind": "glyph_run",
        "object_id": 17,
        "z_order": -2,
        "bounds": [1, 2, 3, 4],
        "foreground": [5, 6, 7, 8],
        "background": [9, 10, 11, 12],
        "attributes": 0x40,
        "text": "visible",
    }
    assert "labels" not in wire["retained"]["regions"][0]
    assert "composite" not in repr(wire)

    unsupported = retained_draw_plane_to_wire(plane)
    unsupported["regions"][0]["draws"][0]["attributes"] = 0x10
    with pytest.raises(ValueError, match="unsupported GLYPH_RUN bits"):
        retained_draw_plane_from_wire(unsupported)


def test_display_wire_rejects_bool_in_integer_fields():
    scope = DisplayScope(1, 2, 0, 0, 0, 0, None)
    wire_scope = display_scope_to_wire(scope)
    wire_scope["attachment_epoch"] = True
    with pytest.raises(TypeError, match="not bool"):
        display_scope_from_wire(wire_scope)

    snapshot = TerminalSnapshot(
        1,
        1,
        ((TerminalCell(" ", (0, 0, 0), (0, 0, 0), 0),),),
        0,
        0,
        False,
        False,
    )
    offer = TerminalDisplayOffer(
        1,
        scope,
        snapshot,
        RetainedDrawPlane(False, False, ()),
    )
    wire_offer = display_offer_to_wire(offer)
    wire_offer["offer_id"] = True
    with pytest.raises(TypeError, match="not bool"):
        display_offer_from_wire(wire_offer)

    wire_offer = display_offer_to_wire(offer)
    wire_offer["composite"] = {}
    with pytest.raises(ValueError, match="fields are not exact"):
        display_offer_from_wire(wire_offer)

    wire_snapshot = snapshot_to_wire(snapshot)
    wire_snapshot["runs"][0][0] = True
    with pytest.raises(TypeError, match="not bool"):
        snapshot_from_wire(wire_snapshot)

    wire_plane = retained_draw_plane_to_wire(offer.retained)
    wire_plane["retained_initialized"] = 1
    with pytest.raises(TypeError, match="must be bool"):
        retained_draw_plane_from_wire(wire_plane)


def test_shared_screen_round_trips_the_selected_rich_view():
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
        session._receive_terminal_output(
            TerminalView(
                attachment_epoch=session.rich_terminal_driver.attachment_epoch,
                session_id=7,
                presentation_epoch=1,
                revision=9,
                cols=2,
                rows=2,
                cells=(
                    (
                        Cell(ord("A"), 1, 2, 1 << 6),
                        Cell(ord("B"), 3, 4, 1 << 5),
                    ),
                    (
                        Cell(ord("C"), 5, 6, 0),
                        Cell(ord("D"), 7, 0, 1),
                    ),
                ),
                dirty_spans=(),
                cursor=Cursor(1, 1, True),
            )
        )
        machine = SharedMachine(session)
        result = machine.screen(since=-1)
        restored = snapshot_from_wire(result["snapshot"])

        assert restored.cols == 2 and restored.rows == 2
        assert restored.cursor_col == 1 and restored.cursor_row == 1
        assert restored.cursor_visible
        assert restored.cells[0][0].attrs == 0x80
        assert restored.cells[0][1].attrs == 0x20

        # A committed resize can precede its required replacement snapshot.
        # Status and screen must both continue to describe the retained rich
        # view while the hidden ANSI fallback tracks the new geometry.
        session.rich_terminal_driver.core.select_ansi_geometry(4, 1)
        session._sync_rich_terminal_geometry()
        assert session.visible_geometry == (2, 2)
        assert (session.terminal.cols, session.terminal.rows) == (4, 1)
        assert machine.status(detailed=False)["terminal"] == [2, 2]


def test_shared_screen_tracks_snapshot_and_display_offer_cursors_independently():
    with MachineSession(
        MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2),
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(retained_policy=_retained_policy()),
    ) as session:
        _arm_retained_offer(session)
        machine = SharedMachine(session)
        offer = session.display_offer
        assert offer is not None
        revision = session.revision

        baseline = machine.screen(
            since=revision,
            since_offer=0,
            display_authorized=False,
        )
        assert baseline == {"changed": False, "revision": revision}

        offered = machine.screen(
            since=revision,
            since_offer=0,
            display_authorized=True,
        )
        assert offered["changed"]
        assert offered["revision"] == revision
        assert offered["generation"] == 0
        assert display_offer_from_wire(offered["display_offer"]) == offer

        unchanged = machine.screen(
            since=revision,
            since_offer=offer.offer_id,
            display_authorized=True,
        )
        assert unchanged == {
            "changed": False,
            "revision": revision,
            "generation": 0,
        }
        ahead_cursor = machine.screen(
            since=revision,
            since_offer=offer.offer_id + 1,
            display_authorized=True,
        )
        assert ahead_cursor["changed"]
        assert ahead_cursor["display_offer"]["offer_id"] == offer.offer_id

        with pytest.raises(TypeError, match="not bool"):
            machine.screen(since=True)
        with pytest.raises(TypeError, match="not bool"):
            machine.screen(since_offer=True)


def test_shared_raw_uses_absolute_bounded_cursors_across_reset():
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
        machine = SharedMachine(session)
        session._receive_rich_terminal_ansi(b"abcdef")

        rolled = machine.raw(since=0)
        assert rolled == {
            "start": 2,
            "available_from": 2,
            "offset": 6,
            "truncated": True,
            "text": "cdef",
            "data_base64": base64.b64encode(b"cdef").decode("ascii"),
        }
        assert machine.raw(since=4)["text"] == "ef"

        session.reset()
        cleared = machine.raw(since=4)
        assert cleared["available_from"] == 6
        assert cleared["start"] == 6
        assert cleared["offset"] == 6
        assert cleared["truncated"]
        assert cleared["text"] == ""


@pytest.mark.parametrize(
    ("returned", "expected", "accepted"),
    (
        (None, DriverStatus.PROGRESS, True),
        (DriverStatus.PROGRESS, DriverStatus.PROGRESS, True),
        (DriverStatus.BACKPRESSURED, DriverStatus.BACKPRESSURED, False),
        (DriverStatus.INVALID, DriverStatus.INVALID, False),
        (DriverStatus.STALE, DriverStatus.STALE, False),
        (DriverStatus.FAILED, DriverStatus.FAILED, False),
    ),
)
def test_shared_input_reports_exact_admission_status(
    monkeypatch,
    returned,
    expected,
    accepted,
):
    with MachineSession(MegapadSystem(ram_size=64 * 1024)) as session:
        machine = SharedMachine(session)
        monkeypatch.setattr(session, "send_text", lambda text: returned)
        monkeypatch.setattr(session, "send_key", lambda key: returned)
        monkeypatch.setattr(session, "resize", lambda cols, rows: returned)

        text = machine.send_text("é")
        key = machine.send_key("enter")
        resize = machine.resize(2, 2)

        assert text == {
            "status": expected.value,
            "accepted_bytes": 2 if accepted else 0,
        }
        assert key == {
            "status": expected.value,
            "accepted_events": 1 if accepted else 0,
        }
        assert resize["status"] == expected.value
        assert resize["accepted"] is accepted
        assert resize["requested"] == [2, 2]
        assert (resize["cols"], resize["rows"]) == session.visible_geometry
        if expected in {DriverStatus.STALE, DriverStatus.FAILED}:
            assert machine.paused
            assert machine.last_error is not None
        else:
            assert not machine.paused


def test_session_dispatch_rejects_input_from_an_old_reset_generation(tmp_path):
    with MachineSession(MegapadSystem(ram_size=64 * 1024)) as session:
        machine = SharedMachine(session)
        server = SessionServer(machine, str(tmp_path / "unused.sock"))
        reset = machine.reset(paused=True)
        generation = reset["generation"]
        before = session.system.uart.rx_pending

        stale = server.dispatch(
            "send_text",
            {"text": "old", "generation": generation - 1},
        )
        assert stale == {"status": "stale_generation", "accepted_bytes": 0}
        assert session.system.uart.rx_pending == before

        accepted = server.dispatch(
            "send_text",
            {"text": "new", "generation": generation},
        )
        assert accepted == {"status": "progress", "accepted_bytes": 3}
        assert session.system.uart.rx_pending == before + 3

        with pytest.raises(ValueError, match="requires generation"):
            server.dispatch("send_key", {"key": "enter"})


def test_session_server_display_lease_binds_delivery_present_input_and_takeover(
    tmp_path,
    monkeypatch,
):
    with MachineSession(
        MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2),
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(retained_policy=_retained_policy()),
    ) as session:
        original_composite = _arm_retained_offer(session)
        machine = SharedMachine(session)
        server = SessionServer(machine, str(tmp_path / "unused.sock"))
        offer = session.display_offer
        assert offer is not None
        assert machine.status(detailed=False)["rich_terminal"][
            "display_required"
        ] is True
        proof = {
            "display_offer_id": offer.offer_id,
            "display_scope": display_scope_to_wire(offer.scope),
        }
        generation = 0

        assert server.dispatch("claim_display", {}, connection_id=1) == {
            "status": "claimed",
            "claimed": True,
        }
        assert server.dispatch("claim_display", {}, connection_id=1)["claimed"]
        assert server.dispatch("claim_display", {}, connection_id=2) == {
            "status": "display_busy",
            "claimed": False,
        }
        observer = server.dispatch(
            "screen",
            {"since": session.revision, "since_offer": 0},
            connection_id=2,
        )
        assert observer == {"changed": False, "revision": session.revision}

        guessed = server.dispatch(
            "present",
            {"generation": generation, **proof},
            connection_id=1,
        )
        assert guessed == {"status": "stale_display", "presented": False}
        assert session.display_offer is offer

        calls = []

        def send_text(text):
            calls.append(text)
            return DriverStatus.PROGRESS

        monkeypatch.setattr(session, "send_text", send_text)
        before_ack = server.dispatch(
            "send_text",
            {"text": "held", "generation": generation, **proof},
            connection_id=1,
        )
        assert before_ack == {"status": "backpressured", "accepted_bytes": 0}
        assert calls == []
        nonholder = server.dispatch(
            "send_text",
            {"text": "foreign", "generation": generation, **proof},
            connection_id=2,
        )
        assert nonholder == {"status": "stale_display", "accepted_bytes": 0}
        assert calls == []

        delivered = server.dispatch(
            "screen",
            {"since": session.revision, "since_offer": 0},
            connection_id=1,
        )
        assert delivered["changed"]
        assert display_offer_from_wire(delivered["display_offer"]) == offer

        wrong_scope = display_scope_to_wire(
            replace(offer.scope, session_id=offer.scope.session_id + 1)
        )
        wrong = server.dispatch(
            "present",
            {
                "generation": generation,
                "display_offer_id": offer.offer_id,
                "display_scope": wrong_scope,
            },
            connection_id=1,
        )
        assert wrong == {"status": "stale_display", "presented": False}
        stale_generation = server.dispatch(
            "present",
            {"generation": generation + 1, **proof},
            connection_id=1,
        )
        assert stale_generation == {
            "status": "stale_generation",
            "presented": False,
        }
        foreign_present = server.dispatch(
            "present",
            {"generation": generation, **proof},
            connection_id=2,
        )
        assert foreign_present == {"status": "stale_display", "presented": False}

        before_present_revision = session.revision
        presented = server.dispatch(
            "present",
            {"generation": generation, **proof},
            connection_id=1,
        )
        assert presented == {
            "status": "presented",
            "presented": True,
            "revision": before_present_revision + 1,
        }
        assert session.displayed_output_view is original_composite
        assert session.last_acknowledged_display_offer == (
            offer.offer_id,
            offer.scope,
        )
        duplicate = server.dispatch(
            "present",
            {"generation": generation, **proof},
            connection_id=1,
        )
        assert duplicate == {
            "status": "duplicate",
            "presented": True,
            "revision": presented["revision"],
        }

        wrong_input = server.dispatch(
            "send_text",
            {
                "text": "stale",
                "generation": generation,
                "display_offer_id": offer.offer_id + 1,
                "display_scope": display_scope_to_wire(offer.scope),
            },
            connection_id=1,
        )
        assert wrong_input == {"status": "stale_display", "accepted_bytes": 0}
        accepted = server.dispatch(
            "send_text",
            {"text": "owned", "generation": generation, **proof},
            connection_id=1,
        )
        assert accepted == {"status": "progress", "accepted_bytes": 5}
        assert calls == ["owned"]

        keys = []
        sizes = []
        monkeypatch.setattr(
            session,
            "send_key",
            lambda key: keys.append(key) or DriverStatus.PROGRESS,
        )
        monkeypatch.setattr(
            session,
            "resize",
            lambda cols, rows: sizes.append((cols, rows)) or DriverStatus.PROGRESS,
        )
        assert server.dispatch(
            "send_key",
            {"key": "enter", "generation": generation, **proof},
            connection_id=1,
        ) == {"status": "progress", "accepted_events": 1}
        resized = server.dispatch(
            "resize",
            {"cols": 3, "rows": 1, "generation": generation, **proof},
            connection_id=1,
        )
        assert resized["status"] == "progress"
        assert resized["accepted"]
        assert resized["requested"] == [3, 1]
        assert keys == ["enter"]
        assert sizes == [(3, 1)]

        baseline = session.snapshot()
        acknowledged_revision = session.revision
        assert server._release_display_holder(1)
        assert session.display_offer is None
        assert session.displayed_output_view is None
        assert session.last_acknowledged_display_offer is None
        assert session.snapshot() == baseline
        assert session.revision == acknowledged_revision

        assert server.dispatch("claim_display", {}, connection_id=2)["claimed"]
        assert session._service_display_cadence()
        replacement = session.display_offer
        assert replacement is not None
        assert replacement.offer_id > offer.offer_id
        takeover = server.dispatch(
            "screen",
            {
                "since": acknowledged_revision,
                "since_offer": offer.offer_id,
            },
            connection_id=2,
        )
        assert takeover["changed"]
        assert takeover["display_offer"]["offer_id"] == replacement.offer_id

        with pytest.raises(TypeError, match="not bool"):
            server.dispatch(
                "present",
                {
                    "generation": generation,
                    "display_offer_id": True,
                    "display_scope": display_scope_to_wire(replacement.scope),
                },
                connection_id=2,
            )


def test_display_holder_disconnect_requeues_for_a_successor(tmp_path):
    scope = DisplayScope(1, 2, 0, 0, 0, 0, None)
    snapshot = TerminalSnapshot(
        1,
        1,
        ((TerminalCell("X", (7, 7, 7), (0, 0, 0), 0),),),
        0,
        0,
        True,
        False,
    )
    plane = RetainedDrawPlane(False, False, ())

    class LeaseMachine:
        def __init__(self):
            self.offer = TerminalDisplayOffer(1, scope, snapshot, plane)
            self.revoke_calls = 0
            self.stopped = False

        def start(self):
            pass

        def stop(self):
            self.stopped = True

        def screen(
            self,
            since=-1,
            *,
            since_offer=0,
            display_authorized=False,
        ):
            result = {"changed": False, "revision": 0}
            if display_authorized:
                result["generation"] = 1
                if since_offer != self.offer.offer_id:
                    result["changed"] = True
                    result["display_offer"] = display_offer_to_wire(self.offer)
            return result

        def present(self, offer_id, offered_scope, *, generation):
            assert generation == 1
            assert (offer_id, offered_scope) == (
                self.offer.offer_id,
                self.offer.scope,
            )
            return {"status": "presented", "presented": True, "revision": 0}

        def revoke_physical_display(self):
            self.revoke_calls += 1
            self.offer = replace(self.offer, offer_id=self.offer.offer_id + 1)
            return True

    socket_path = tmp_path / "display-takeover.sock"
    machine = LeaseMachine()
    server = SessionServer(machine, str(socket_path))
    try:
        server.serve_in_thread()
    except PermissionError:
        pytest.skip("Unix sockets are unavailable in this sandbox")

    successor = SessionClient(str(socket_path))
    try:
        owner = SessionClient(str(socket_path))
        assert owner.request("claim_display")["claimed"]
        first = owner.request("screen", since=0, since_offer=0)["display_offer"]
        assert owner.request(
            "present",
            generation=1,
            display_offer_id=first["offer_id"],
            display_scope=first["scope"],
        )["presented"]
        assert not successor.request("claim_display")["claimed"]

        owner.close()
        claimed = wait_until(
            lambda: (
                result
                if (result := successor.request("claim_display"))["claimed"]
                else None
            )
        )
        assert claimed["status"] == "claimed"
        assert machine.revoke_calls == 1
        takeover = successor.request(
            "screen",
            since=0,
            since_offer=first["offer_id"],
        )
        assert takeover["changed"]
        assert takeover["revision"] == 0
        assert takeover["display_offer"]["offer_id"] > first["offer_id"]
    finally:
        successor.close()
        server.stop()

    assert machine.stopped
    assert not socket_path.exists()


@pytest.mark.parametrize("rich_terminal", (None, _rich_terminal_config()))
def test_shared_paused_step_counts_only_guest_instructions(
    monkeypatch,
    rich_terminal,
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
        rich_terminal=rich_terminal,
    ) as session:
        machine = SharedMachine(session)
        machine.paused = True
        results = iter(
            (
                SystemRunStats(
                    instructions_executed=0,
                    system_cycles_advanced=7,
                    per_core_instructions=(0,),
                    per_core_cycles=(7,),
                    system_stop_reason="host_backpressure",
                ),
                SystemRunStats(
                    instructions_executed=1,
                    system_cycles_advanced=11,
                    per_core_instructions=(1,),
                    per_core_cycles=(11,),
                    system_stop_reason="instruction_limit",
                ),
            )
        )
        monkeypatch.setattr(session, "run_batch_stats", lambda count: next(results))

        blocked = machine.step(count=3)
        assert blocked["executed"] == 0
        assert blocked["cycles"] == 7
        assert blocked["stop_reason"] == "host_backpressure"
        assert machine.total_steps == 0

        progressed = machine.step(count=1)
        assert progressed["executed"] == 1
        assert progressed["cycles"] == 11
        assert progressed["stop_reason"] == "instruction_limit"
        assert machine.total_steps == 1


def test_shared_lost_session_requires_successful_reset():
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
        machine = SharedMachine(session)
        machine.paused = True
        session.rich_terminal_driver.close()

        status = machine.status(detailed=False)
        assert status["state"] == "lost"
        assert not status["idle"]
        assert status["rich_terminal"]["lost"]
        with pytest.raises(RuntimeError, match="requires a machine reset"):
            machine.resume()
        with pytest.raises(RuntimeError, match="requires a machine reset"):
            machine.step()

        reset = machine.reset(paused=True)
        assert reset["state"] == "paused"
        assert not reset["rich_terminal"]["lost"]
        assert reset["rich_terminal"]["failure"] is None


def test_shared_failed_reset_remains_paused_and_visible(monkeypatch):
    with MachineSession(MegapadSystem(ram_size=64 * 1024)) as session:
        machine = SharedMachine(session)
        machine.paused = False

        def fail_reset():
            raise RuntimeError("injected reset failure")

        monkeypatch.setattr(session, "reset", fail_reset)
        with pytest.raises(RuntimeError, match="injected reset failure"):
            machine.reset(paused=False)

        assert machine.paused
        assert machine.last_error == "RuntimeError: injected reset failure"


def test_shared_machine_wakes_idle_cpu_for_timer_irq():
    session = MachineSession.from_bios(BIOS)
    machine = SharedMachine(
        session,
        idle_tick_cycles=1_000,
        idle_sleep_s=0.001,
    )
    machine.start()
    try:
        wait_until(lambda: session.system.all_idle_or_halted)
        before = machine.total_steps
        with machine.condition:
            cpu = session.system.cpu
            timer = session.system.timer
            cpu.flag_i = True
            timer.counter = 0
            timer.compare = 1_000
            timer.control = 0x03
            machine.condition.notify_all()

        wait_until(lambda: machine.total_steps > before)
        assert machine.last_error is None
    finally:
        machine.stop()


def test_lightweight_status_skips_forth_diagnostics(monkeypatch):
    with MachineSession.from_bios(BIOS) as session:
        machine = SharedMachine(session)
        calls = []

        def diagnostics(cpu):
            calls.append(cpu)
            return {"sentinel": True}

        monkeypatch.setattr(machine, "_forth_diagnostics", diagnostics)

        lightweight = machine.status(detailed=False)
        assert calls == []
        assert "protocol" not in lightweight
        assert "state" in lightweight
        assert "steps" in lightweight
        assert "revision" in lightweight
        assert "forth" not in lightweight
        assert "cpu" not in lightweight
        assert "nic" not in lightweight
        assert "host_profile" not in lightweight
        assert lightweight["rich_terminal"]["display_required"] is False
        assert lightweight["rich_terminal"]["machine_publications"] == 0
        assert lightweight["rich_terminal"]["machine_publication_bytes"] == 0
        assert lightweight["rich_terminal"]["frames"] == 0
        assert lightweight["rich_terminal"]["frame_bytes"] == 0
        assert lightweight["rich_terminal"]["frames_by_type"] == {}
        assert lightweight["rich_terminal"]["frame_bytes_by_type"] == {}
        assert lightweight["rich_terminal"]["decoder_buffered_bytes"] == 0

        detailed = machine.status()
        assert calls == [session.system.cpu]
        assert detailed["forth"] == {"sentinel": True}
        assert "cpu" in detailed
        assert "nic" in detailed
        assert "host_profile" not in detailed


def test_lightweight_status_exposes_rich_transport_counters() -> None:
    with MachineSession(
        MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2),
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(),
    ) as session:
        driver = session.rich_terminal_driver
        assert driver is not None
        core = driver.core
        core._machine_publications_received = 7
        core._machine_publication_bytes_received = 700
        core._frames_received = 5
        core._frame_bytes_received = 500
        core._frames_received_by_type = {0x0110: 1, 0x0101: 4}
        core._frame_bytes_received_by_type = {0x0110: 56, 0x0101: 444}

        rich = SharedMachine(session).status(detailed=False)["rich_terminal"]

        assert rich["machine_publications"] == 7
        assert rich["machine_publication_bytes"] == 700
        assert rich["frames"] == 5
        assert rich["frame_bytes"] == 500
        assert rich["frames_by_type"] == {"0x0101": 4, "0x0110": 1}
        assert rich["frame_bytes_by_type"] == {
            "0x0101": 444,
            "0x0110": 56,
        }
        assert rich["decoder_buffered_bytes"] == 0


def test_forth_diagnostics_reach_words_beyond_the_old_depth_ceiling():
    stride = 48
    base = 0x100
    latest_variable = 0x20
    here_variable = 0x28
    names = ["ANCIENT-CURRENT", "ANCIENT-TARGET"] + ["N"] * 16_384
    here = base + len(names) * stride
    data_stack = here + 0x40
    return_stack = here + 0x80
    memory = bytearray(here + 0x100)

    def write64(address, value):
        memory[address:address + 8] = int(value).to_bytes(8, "little")

    locations = {}
    previous = 0
    for index, name in enumerate(names):
        header = base + index * stride
        encoded = name.encode("ascii")
        write64(header, previous)
        memory[header + 8] = len(encoded)
        memory[header + 9:header + 9 + len(encoded)] = encoded
        locations.setdefault(name, (header, header + 9 + len(encoded)))
        previous = header

    write64(latest_variable, previous)
    write64(here_variable, here)
    current_header, current_code = locations["ANCIENT-CURRENT"]
    target_header, target_code = locations["ANCIENT-TARGET"]
    write64(return_stack, target_code + 2)

    def read8(address):
        if not 0 <= address < len(memory):
            raise IndexError(address)
        return memory[address]

    def read64(address):
        if not 0 <= address <= len(memory) - 8:
            raise IndexError(address)
        return int.from_bytes(memory[address:address + 8], "little")

    registers = [0] * 32
    registers[3] = current_code + 1
    registers[14] = data_stack
    registers[15] = return_stack
    cpu = SimpleNamespace(
        mem_read8=read8,
        mem_read64=read64,
        regs=registers,
        pc=0,
    )
    machine = object.__new__(SharedMachine)
    machine.lock = threading.RLock()
    machine.session = SimpleNamespace(
        bios_labels={
            "var_latest": latest_variable,
            "var_here": here_variable,
        },
        system=SimpleNamespace(
            cpu=cpu,
            ram_size=len(memory),
            ext_mem_size=0,
            ext_mem_base=0,
            ext_mem_end=0,
        ),
    )

    named = machine.forth(["ancient-target"])
    detailed = machine._forth_diagnostics(cpu)

    assert named["words"]["ANCIENT-TARGET"] == {
        "name": "ANCIENT-TARGET",
        "header": target_header,
        "code": target_code,
    }
    assert detailed["word"] == {
        "name": "ANCIENT-CURRENT",
        "header": current_header,
        "code": current_code,
        "offset": 1,
    }
    assert detailed["return_words"][0] == {
        "name": "ANCIENT-TARGET",
        "header": target_header,
        "code": target_code,
        "offset": 2,
    }


def test_forth_diagnostics_follow_cross_bank_links_without_aliasing_gaps():
    ram_size = 0x1000
    ext_base = ram_size
    ext_size = 0x1000
    latest_variable = 0x20
    here_variable = 0x28
    data_stack = 0x80
    return_stack = 0xC0
    ram = bytearray(ram_size)
    external = bytearray(ext_size)
    reads = []

    def resolve(address, count):
        if 0 <= address and address + count <= ram_size:
            return ram, address
        if ext_base <= address and address + count <= ext_base + ext_size:
            return external, address - ext_base
        raise IndexError(address)

    def write64(address, value):
        memory, offset = resolve(address, 8)
        memory[offset:offset + 8] = int(value).to_bytes(8, "little")

    def write_word(header, link, name, variable_value=None):
        encoded = name.encode("ascii")
        memory, offset = resolve(header, 9 + len(encoded))
        memory[offset:offset + 8] = int(link).to_bytes(8, "little")
        memory[offset + 8] = len(encoded)
        memory[offset + 9:offset + 9 + len(encoded)] = encoded
        code = header + 9 + len(encoded)
        if variable_value is not None:
            data_address = code + 17
            memory, offset = resolve(code, 17)
            memory[offset:offset + 3] = b"\xf0\x60\x10"
            memory[offset + 3:offset + 11] = data_address.to_bytes(8, "little")
            memory[offset + 11:offset + 17] = b"\x67\xe0\x08\x54\xe1\x0e"
            write64(data_address, variable_value)
        return code

    def read8(address):
        reads.append((address, 1))
        memory, offset = resolve(address, 1)
        return memory[offset]

    def read64(address):
        reads.append((address, 8))
        memory, offset = resolve(address, 8)
        return int.from_bytes(memory[offset:offset + 8], "little")

    old_header = 0x80
    system_here_header = 0x100
    saved_here_header = 0x180
    external_header = ext_base + 0x100
    shadow_header = ext_base + 0x200
    current_header = 0x300
    system_here = 0x380
    user_here = ext_base + 0x300
    shadow_here = ext_base + 0x280
    old_code = write_word(old_header, 0, "BANK-OLD")
    write_word(system_here_header, old_header, "SYS-HERE-SAVE", system_here)
    write_word(
        saved_here_header,
        system_here_header,
        "U-DICT-HERE",
        user_here,
    )
    external_code = write_word(
        external_header,
        saved_here_header,
        "EXT-MIDDLE",
    )
    write_word(shadow_header, external_header, "U-DICT-HERE", shadow_here)
    current_code = write_word(current_header, shadow_header, "BANK-CURRENT")
    write64(latest_variable, current_header)
    write64(here_variable, system_here)
    write64(return_stack, old_code + 2)

    registers = [0] * 32
    registers[3] = external_code + 1
    registers[14] = data_stack
    registers[15] = return_stack
    cpu = SimpleNamespace(
        mem_read8=read8,
        mem_read64=read64,
        regs=registers,
        pc=0,
    )
    machine = object.__new__(SharedMachine)
    machine.lock = threading.RLock()
    machine.session = SimpleNamespace(
        bios_labels={
            "var_latest": latest_variable,
            "var_here": here_variable,
        },
        system=SimpleNamespace(
            cpu=cpu,
            ram_size=ram_size,
            ext_mem_size=ext_size,
            ext_mem_base=ext_base,
            ext_mem_end=ext_base + ext_size,
        ),
    )

    named = machine.forth(["bank-current", "ext-middle", "bank-old"])
    detailed = machine._forth_diagnostics(cpu)

    assert set(named["words"]) == {"BANK-CURRENT", "EXT-MIDDLE", "BANK-OLD"}
    assert named["words"]["BANK-CURRENT"]["code"] == current_code
    assert detailed["word"] == {
        "name": "EXT-MIDDLE",
        "header": external_header,
        "code": external_code,
        "offset": 1,
    }
    assert detailed["return_words"][0] == {
        "name": "BANK-OLD",
        "header": old_header,
        "code": old_code,
        "offset": 2,
    }

    write64(current_header, external_header)
    write64(shadow_header, current_header)
    write64(latest_variable, shadow_header)
    write64(here_variable, system_here)
    left_userland, bank_here = machine._forth_dictionary(cpu)
    external_word = next(
        word for word in left_userland if word["header"] == shadow_header
    )

    assert bank_here == system_here
    assert external_word["_upper"] == user_here
    assert machine._forth_word_at(left_userland, bank_here, user_here + 8) is None

    write64(here_variable, user_here)
    reentered_userland, external_here = machine._forth_dictionary(cpu)
    bank_word = next(
        word for word in reentered_userland if word["name"] == "BANK-CURRENT"
    )

    assert external_here == user_here
    assert bank_word["_upper"] == system_here
    assert (
        machine._forth_word_at(reentered_userland, external_here, system_here + 8)
        is None
    )

    write64(latest_variable, current_header)
    write64(current_header, shadow_header)
    write64(shadow_header, external_header)
    write64(external_header, saved_here_header)
    write64(here_variable, system_here)

    gap_address = 0x8000
    write64(current_header, gap_address)
    reads.clear()
    partial, incomplete_here = machine._forth_dictionary(cpu)

    assert [word["name"] for word in partial] == ["BANK-CURRENT"]
    assert incomplete_here == 0
    assert all(address != gap_address for address, _count in reads)

    overlapping_address = current_header + 0x40
    write64(current_header, overlapping_address)
    reads.clear()
    partial, incomplete_here = machine._forth_dictionary(cpu)

    assert [word["name"] for word in partial] == ["BANK-CURRENT"]
    assert incomplete_here == 0
    assert all(address != overlapping_address for address, _count in reads)

    write64(current_header, external_header)
    write64(external_header, current_header)
    partial, incomplete_here = machine._forth_dictionary(cpu)

    assert [word["name"] for word in partial] == ["BANK-CURRENT", "EXT-MIDDLE"]
    assert incomplete_here == 0


def test_opt_in_host_profile_is_detailed_only_and_restarts_on_reset() -> None:
    with MachineSession.from_bios(BIOS) as session:
        machine = SharedMachine(session, host_profile=True)
        machine.paused = True
        machine.start()
        try:
            lightweight = machine.status(detailed=False)
            assert "host_profile" not in lightweight

            detailed = machine.status()
            first = detailed["host_profile"]
            assert first["schema_version"] == 17
            assert first["enabled"]
            assert first["generation"] > 0
            assert first["single_core_block_cache"] == {
                "kind": "set-associative-exact-icache-span",
                "sets": 1_024,
                "ways": 4,
                "entries": 4_096,
                "identity_bytes": 16,
            }
            assert first["single_core_block_rejection_cache"] == {
                "kind": "set-associative-exact-icache-span",
                "sets": 512,
                "ways": 4,
                "entries": 2_048,
                "identity_bytes": 16,
            }
            region_storage = first["single_core_jit_region_storage"]
            assert isinstance(region_storage["enabled"], bool)
            assert not region_storage["ready"]
            assert not region_storage["failed"]
            assert region_storage["slot_count"] == 0
            assert region_storage["slot_bytes"] == 0
            assert region_storage["mapped_bytes_per_alias"] == 0
            assert first["single_core_jit_successor_profile"] == {
                "kind": "bounded-set-associative-space-saving",
                "scope": (
                    "consecutive-complete-helper-free-register-control-"
                    "x86_64-blocks-within-one-uncontended-segment"
                ),
                "sets": 1_024,
                "ways": 8,
                "entries": 8_192,
                "candidate_block_completions": 0,
                "observations": 0,
                "replacements": 0,
                "exact": True,
                "counter_saturated": False,
                "edges": [],
            }

            reset = machine.reset(paused=True)
            restarted = reset["host_profile"]
            assert restarted["enabled"]
            assert restarted["generation"] == first["generation"] + 1
            assert restarted["counts"]["batches"] == 0
            assert (
                restarted["single_core_jit_region_storage"]
                == first["single_core_jit_region_storage"]
            )
            assert (
                restarted["single_core_jit_successor_profile"]
                == first["single_core_jit_successor_profile"]
            )
        finally:
            machine.stop()


def test_ping_reports_only_liveness():
    with MachineSession.from_bios(BIOS) as session:
        server = SessionServer(SharedMachine(session), "unused.sock")
        result = server.dispatch("ping", {})

    assert set(result) == {"time"}
    assert isinstance(result["time"], float)


def test_screen_encodes_snapshot_outside_machine_lock(monkeypatch):
    with MachineSession.from_bios(BIOS, cols=40, rows=12) as session:
        machine = SharedMachine(session)
        conversion_started = threading.Event()
        allow_conversion = threading.Event()
        original = snapshot_to_wire

        def blocking_conversion(snapshot):
            conversion_started.set()
            assert allow_conversion.wait(timeout=2.0)
            return original(snapshot)

        monkeypatch.setattr(
            "shared_session.snapshot_to_wire",
            blocking_conversion,
        )
        result = []
        failure = []

        def request_screen():
            try:
                result.append(machine.screen(since=-1))
            except BaseException as exc:  # propagate worker failures below
                failure.append(exc)

        worker = threading.Thread(target=request_screen)
        worker.start()
        assert conversion_started.wait(timeout=2.0)
        acquired = machine.lock.acquire(timeout=0.5)
        if acquired:
            machine.lock.release()
        allow_conversion.set()
        worker.join(timeout=2.0)

        assert acquired, "screen RLE conversion held the machine lock"
        assert not worker.is_alive()
        assert failure == []
        assert result[0]["changed"]
        assert snapshot_from_wire(result[0]["snapshot"]).cols == 40


def test_screen_encodes_display_offer_outside_machine_lock(monkeypatch):
    with MachineSession(
        MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2),
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(retained_policy=_retained_policy()),
    ) as session:
        _arm_retained_offer(session)
        machine = SharedMachine(session)
        conversion_started = threading.Event()
        allow_conversion = threading.Event()
        original = display_offer_to_wire

        def blocking_conversion(offer):
            conversion_started.set()
            assert allow_conversion.wait(timeout=2.0)
            return original(offer)

        monkeypatch.setattr(
            "shared_session.display_offer_to_wire",
            blocking_conversion,
        )
        result = []
        failure = []

        def request_screen():
            try:
                result.append(
                    machine.screen(
                        since=session.revision,
                        since_offer=0,
                        display_authorized=True,
                    )
                )
            except BaseException as exc:  # propagate worker failures below
                failure.append(exc)

        worker = threading.Thread(target=request_screen)
        worker.start()
        assert conversion_started.wait(timeout=2.0)
        acquired = machine.lock.acquire(timeout=0.5)
        if acquired:
            machine.lock.release()
        allow_conversion.set()
        worker.join(timeout=2.0)

        assert acquired, "display-offer conversion held the machine lock"
        assert not worker.is_alive()
        assert failure == []
        assert result[0]["changed"]
        assert display_offer_from_wire(result[0]["display_offer"]) == session.display_offer


def test_shared_server_clients_control_one_machine(tmp_path):
    socket_path = tmp_path / "shared.sock"
    session = MachineSession.from_bios(BIOS, cols=60, rows=20)
    machine = SharedMachine(session)
    server = SessionServer(machine, str(socket_path))
    try:
        server.serve_in_thread()
    except PermissionError:
        session.close()
        pytest.skip("Unix sockets are unavailable in this sandbox")

    try:
        with SessionClient(str(socket_path)) as controller, \
                SessionClient(str(socket_path)) as viewer:
            status = wait_until(
                lambda: (
                    current
                    if ((current := controller.request("status"))["state"] == "idle"
                        and current["clients"] == 2)
                    else None
                )
            )
            assert status["clients"] == 2
            generation = status["generation"]
            assert status["clock"]["mode"] == "virtual"
            assert status["cpu"]["cycles"] >= 0
            assert len(status["cpu"]["registers"]) == 32
            assert "return_words" in status["forth"]
            lightweight = viewer.request("status", detailed=False)
            assert lightweight["clients"] == 2
            assert lightweight["state"] == "idle"
            assert "forth" not in lightweight
            assert "cpu" not in lightweight
            network = controller.request("network")
            assert network["backend"] == "loopback"
            assert network["guest_rx_queued"] == 0
            forth = controller.request("forth", names=["STATE"])
            assert forth["words"]["STATE"]["name"] == "STATE"
            peek = controller.request("peek", address=0, count=2)
            assert peek["cell_size"] == 8
            assert len(peek["values"]) == 2

            initial = viewer.request("screen", since=-1)
            assert initial["changed"]
            revision = initial["revision"]
            assert not viewer.request("screen", since=revision)["changed"]

            controller.request(
                "send_text",
                text="6 7 * .\n",
                generation=generation,
            )
            raw = wait_until(
                lambda: (
                    result
                    if "42 " in (result := controller.request("raw", since=0))["text"]
                    else None
                )
            )
            assert "42  ok" in raw["text"]

            updated = wait_until(
                lambda: (
                    result
                    if (result := viewer.request("screen", since=revision))["changed"]
                    else None
                )
            )
            snapshot = snapshot_from_wire(updated["snapshot"])
            assert snapshot.find("42")

            paused = controller.request("pause")
            assert paused["paused"]
            stepped = controller.request("step", count=1)
            assert stepped["executed"] == 1
            assert stepped["status"]["paused"]
            assert not controller.request("resume")["paused"]

            resized = controller.request(
                "resize",
                cols=72,
                rows=24,
                generation=generation,
            )
            assert resized["cols"] == 72
            resized_screen = viewer.request("screen", since=updated["revision"])
            assert resized_screen["snapshot"]["cols"] == 72
            assert resized_screen["snapshot"]["rows"] == 24

            png = tmp_path / "shared.png"
            capture = controller.request("capture", png=str(png))
            assert Path(capture["outputs"]["png"]).is_file()

            reset = controller.request("reset", paused=False)
            assert not reset["paused"]
            wait_until(lambda: controller.request("status")["state"] == "idle")
            assert "42" not in controller.request("text")["text"]
    finally:
        server.stop()

    assert not socket_path.exists()
