"""Real retained publication and input across a declared hybrid transition.

The complete guest module performs negotiation, publication and event decoding.
Only test-command selection uses host memory writes, while the paused owner is
between semantic boundaries. Display delivery, acknowledgment and both input
kinds use the ordinary shared-session dispatcher, without a socket listener.
"""

from dataclasses import replace

import pytest

from asm import assemble
from hybrid.manifest import RoutineDeclaration
from hybrid.runtime import HybridRuntime
from hybrid.session import HybridSession, HybridSharedMachine
from rich_terminal.driver import DriverLimits
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlEventKind, ControlWireDefinition, RegionWireDefinition,
    encode_control_definition, encode_region_definition,
)
from rich_terminal.semantic_fields import (
    FieldContent, FieldFlag, FieldKind, FieldRect, encode_field_content,
)
from rich_terminal.transport import EgressWatermarks, HostPortLimits
from shared.session import RichTerminalSessionConfig
from shared_session import (
    SessionServer, display_offer_from_wire, display_scope_to_wire,
)
from simulator.runtime import _MachineCursor
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    LIVE_HANDSHAKE_SCENARIO_SOURCE, ONE_CORE_UART_LOCK_SHIMS,
    SIMULATOR_SOURCE_MAX_STEPS, _rich_terminal_module_source, _stored_cell,
)
from tests.test_rich_terminal_semantic_server import _config, _policy


CONNECTION = 17
OWNER = 7
GENERATION = 1
TASK_ID = 2
FIELD_ID = 3
FIELD_REVISION = 7
VISIBLE_ENABLED = ControlState.VISIBLE | ControlState.ENABLED


def _definitions():
    content = FieldContent(
        FIELD_REVISION, FieldKind.INTEGER, FieldFlag(0),
        FieldRect(0, 0, 1, 1), FieldRect(1, 0, 1, 1),
        value=441, minimum=40, maximum=2000, step=10,
    )
    return (
        ControlWireDefinition(
            OWNER, GENERATION, 1, ControlKind.TASKBAR, VISIBLE_ENABLED, 0,
            1, 0, 0, ObjectBounds(0, 0, 2, 1), "", "",
        ),
        ControlWireDefinition(
            OWNER, GENERATION, TASK_ID, ControlKind.TASK, VISIBLE_ENABLED, 0,
            1, 1, 0, ObjectBounds(0, 0, 2, 1), "Pad", "",
        ),
        ControlWireDefinition(
            OWNER, GENERATION, FIELD_ID, ControlKind.FIELD, VISIBLE_ENABLED, 0,
            1, 0, 0, ObjectBounds(0, 1, 2, 1), "Hz", "", content,
        ),
    )


def _configuration():
    policy = replace(
        _policy(),
        features=(RetainedFeature.CORE | RetainedFeature.CONTROLS
                  | RetainedFeature.TASKBARS | RetainedFeature.FIELDS),
    )
    return RichTerminalSessionConfig(
        host_limits=HostPortLimits(
            egress=EgressWatermarks(32_768, 4_096, 32, 4),
            retained_publication_bytes=16_384,
            ingress_bytes=32_768, ingress_events=64,
            ingress_control_bytes=4_096, ingress_control_events=8,
            geometry_events=2,
        ),
        terminal_config=_config(),
        driver_limits=DriverLimits(32_768, 64),
        ansi_history_bytes=1_024,
        service_batches=4,
        retained_policy=policy,
    )


def _guest_source():
    definitions = _definitions()
    region = RegionWireDefinition(OWNER, GENERATION, 1, 0, 0, 2, 2, 0, 0, 2, 2, 0, 3)
    retained_bytes = 40 + len(encode_region_definition(region)) + sum(
        40 + len(encode_control_definition(item)) for item in definitions
    )
    content = encode_field_content(definitions[-1].content)
    # The existing CELL-handshake fixture uses deliberately tiny storage.
    # Give the production module room for retained discovery and FIELD frames.
    handshake = LIVE_HANDSHAKE_SCENARIO_SOURCE.replace(
        b"_PT-CONTROL-RESERVE _PT-HDR + 32 +", b"8192",
    ).replace(b"_PT-OPEN-BYTES", b"8192")
    content_source = (
        "CREATE HR-CONTENT " + " ".join(f"{byte} C," for byte in content) + "\n"
    ).encode()
    source = f'''
VARIABLE HR-COMMAND VARIABLE HR-DONE VARIABLE HR-ERROR
VARIABLE HR-COMPLETIONS VARIABLE HR-EVENT-COUNT VARIABLE HR-MACHINE-VALUE
VARIABLE HR-RETAINED-READY
CREATE HR-RECORDS 256 ALLOT
: HR-CHECK DUP IF HR-ERROR ! ELSE DROP THEN ;
: HR-POLL
  DBL-POLL-COMPLETION DBL-S PT-COMPLETION-POLL IF
    HR-CHECK
    DBL-POLL-COMPLETION PT-COMPLETION-STATUS@ HR-CHECK
    1 HR-COMPLETIONS +!
  ELSE HR-CHECK THEN
  DBL-POLL-EVENT DBL-S PT-EVENT-POLL IF
    HR-CHECK
    DBL-POLL-EVENT PT-EVENT-TYPE@ PT-EVENT-CONTROL = IF
      HR-EVENT-COUNT @ 4 >= IF 99 HR-ERROR ! EXIT THEN
      HR-EVENT-COUNT @ 64 * HR-RECORDS + >R
      DBL-POLL-EVENT PT-CONTROL-EVENT-OWNER@ R@ !
      DBL-POLL-EVENT PT-CONTROL-EVENT-GENERATION@ R@ 8 + !
      DBL-POLL-EVENT PT-CONTROL-EVENT-ID@ R@ 16 + !
      DBL-POLL-EVENT PT-CONTROL-EVENT-KIND@ R@ 24 + !
      DBL-POLL-EVENT PT-CONTROL-EVENT-MODIFIERS@ R@ 32 + !
      DBL-POLL-EVENT PT-EVENT-REVISION@ R@ 40 + !
      DBL-POLL-EVENT PT-CONTROL-EVENT-CONTENT-REVISION@ R@ 48 + !
      DBL-POLL-EVENT PT-CONTROL-EVENT-ADJUSTMENT@ R> 56 + !
      1 HR-EVENT-COUNT +!
    THEN
  ELSE HR-CHECK THEN ;
: HR-PUBLISH
  2 2 0 0 0 4 {retained_bytes}
    PT-CELL-NONE PT-RET-REPLACE-START DBL-S PT-PRESENT-BEGIN HR-CHECK
  7 1 1 0 0 2 2 0 0 2 2 0 3 DBL-S PT-REGION-DEFINE HR-CHECK
  7 1 1 PT-CONTROL-TASKBAR 3 0 1 0 0
    0 0 2 1 0 0 0 0 0 0 DBL-S PT-CONTROL-DEFINE HR-CHECK
  7 1 2 PT-CONTROL-TASK 3 0 1 1 0
    0 0 2 1 S" Pad" 0 0 0 0 DBL-S PT-CONTROL-DEFINE HR-CHECK
  7 1 3 PT-CONTROL-FIELD 3 0 1 0 0
    0 1 2 1 S" Hz" 0 0 HR-CONTENT {len(content)}
    DBL-S PT-CONTROL-DEFINE HR-CHECK
  PT-COMMIT DBL-S PT-PRESENT-COMMIT HR-CHECK ;
: HR-REVEAL
  2 2 0 0 0 0 0 PT-CELL-NONE PT-RET-REPLACE-CONTINUE
    DBL-S PT-PRESENT-BEGIN HR-CHECK
  PT-COMMIT-AND-REVEAL DBL-S PT-PRESENT-COMMIT HR-CHECK ;
: HR-RUN-COMMAND
  HR-COMMAND @
  DUP 1 = IF DBL-SNAPSHOT DBL-SNAPSHOT-S @ HR-CHECK THEN
  DUP 2 = IF DBL-S PT-RETAINED-DISCOVER HR-CHECK THEN
  DUP 3 = IF 7 1 1 0 3 0 0 32 0 DBL-S PT-OWNER-OPEN HR-CHECK THEN
  DUP 4 = IF HR-PUBLISH THEN
  DUP 5 = IF HR-REVEAL THEN
  DUP 6 = IF 41 H-INC HR-MACHINE-VALUE ! THEN
  0 HR-COMMAND ! HR-DONE ! ;
: HR-ROOT
  DBL-BOOT
  BEGIN DBL-SERVICE HR-POLL
    DBL-S PT-RETAINED-AVAILABLE? HR-RETAINED-READY !
    HR-COMMAND @ IF HR-RUN-COMMAND THEN
  AGAIN ;
'''.encode()
    return handshake + content_source + source


@pytest.mark.parametrize("executor", ("python", "native"))
@pytest.mark.parametrize("machine_quantum", (None, 1))
def test_hybrid_retained_publication_ack_and_guest_control_events(
    tmp_path, executor, machine_quantum,
):
    pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    hybrid = HybridRuntime.create(
        executor=executor,
        geometry={"bank0_size": 1 << 20, "external_size": 1 << 20},
    )
    runtime = hybrid.semantic
    server = None
    yields = machine_quantum is not None
    try:
        _load_exceptions(runtime)
        runtime.evaluate(
            ONE_CORE_UART_LOCK_SHIMS + _rich_terminal_module_source(),
            source_name="hybrid-retained:complete-rich-terminal.f",
            step_budget=SIMULATOR_SOURCE_MAX_STEPS,
        )
        hybrid.register(RoutineDeclaration(
            "H-INC", bytes(assemble("inc r4\nret.l")), input_cells=1, output_cells=1,
        ))
        runtime.evaluate(_guest_source(), source_name="hybrid-retained-caller.f")
        assert runtime.drain_uart_output() == b""
        session = HybridSession(
            hybrid, "HR-ROOT", cols=2, rows=2,
            semantic_quantum_steps=16_384,
            machine_quantum_instructions=machine_quantum,
            rich_terminal=_configuration(),
        )
        backend = session.backend
        machine = HybridSharedMachine(session)
        machine.paused = True
        server = SessionServer(machine, str(tmp_path / "unused.sock"))
        machine.start()
        status = server.dispatch("status", {"detailed": False})
        generation = status["generation"]
        assert status["runtime"]["mode"] == "hybrid"
        assert status["runtime"]["executor"] == executor
        assert runtime.memory.dense_backing is not None
        assert server.dispatch("claim_display", {}, connection_id=CONNECTION)["claimed"]

        def advance_until(predicate):
            for _ in range(200):
                if predicate():
                    return
                result = server.dispatch("step", {"count": 1})
                assert result["status"]["error"] is None, result["status"]
                assert session.rich_terminal_failure is None
                assert _stored_cell(runtime, "HR-ERROR") == 0
            observed = {name: _stored_cell(runtime, name) for name in (
                "HR-DONE", "HR-COMPLETIONS", "HR-EVENT-COUNT", "HR-RETAINED-READY",
                "DBL-STATE", "DBL-REVISION", "DBL-SNAPSHOT-NEEDED",
            )}
            pytest.fail(f"hybrid retained caller did not reach its bounded milestone: {observed}")

        def start_command(number):
            with machine.lock:
                assert _stored_cell(runtime, "HR-COMMAND") == 0
                runtime.memory.write64(runtime.find("HR-COMMAND").body_address, number)

        def command(number):
            start_command(number)
            advance_until(lambda: _stored_cell(runtime, "HR-DONE") == number)

        advance_until(lambda: _stored_cell(runtime, "DBL-ACTIVE") != 0)
        assert _stored_cell(runtime, "DBL-INIT-S") == _stored_cell(runtime, "DBL-START-S") == 0
        command(1)
        # Initial CELL snapshot results settle internally; only later retained
        # transactions/lifecycle requests enter the public completion queue.
        advance_until(lambda: _stored_cell(runtime, "DBL-REVISION") == 1
                      and _stored_cell(runtime, "DBL-SNAPSHOT-NEEDED") == 0)
        assert _stored_cell(runtime, "HR-COMPLETIONS") == 0
        assert session.snapshot().lines() == ["AB", "C "]
        command(2)
        advance_until(lambda: _stored_cell(runtime, "HR-RETAINED-READY") != 0)
        assert session.rich_terminal_driver.core.retained_enabled
        assert session.rich_terminal_driver.core.model_revision == 1
        command(3)
        advance_until(lambda: _stored_cell(runtime, "HR-COMPLETIONS") == 1)
        command(4)
        advance_until(lambda: _stored_cell(runtime, "HR-COMPLETIONS") == 2)
        assert session.display_offer is None
        command(5)
        advance_until(lambda: _stored_cell(runtime, "HR-COMPLETIONS") == 3
                      and session.display_offer is not None)

        delivered = server.dispatch("screen", {"since": -1}, connection_id=CONNECTION)
        offer = display_offer_from_wire(delivered["display_offer"])
        assert offer == session.display_offer
        assert offer.retained.retained_visible and offer.retained.retained_initialized
        scene = session.rich_terminal_driver.core.retained_state
        controls = scene.active.owners[OWNER].controls
        assert {key: item.kind for key, item in controls.items()} == {
            1: ControlKind.TASKBAR, TASK_ID: ControlKind.TASK, FIELD_ID: ControlKind.FIELD,
        }
        assert controls[FIELD_ID].content == _definitions()[-1].content
        proof = dict(generation=generation, display_offer_id=offer.offer_id,
                     display_scope=display_scope_to_wire(offer.scope))
        activation = dict(proof, owner_id=OWNER, owner_generation=GENERATION,
                          control_id=TASK_ID, modifiers=5)
        assert server.dispatch("send_control_event", activation, connection_id=CONNECTION) == {
            "status": "backpressured", "accepted_events": 0,
        }
        presented = server.dispatch("present", proof, connection_id=CONNECTION)
        assert presented["presented"] and presented["status"] == "presented"
        assert session.last_acknowledged_display_offer == (offer.offer_id, offer.scope)

        if not yields:
            command(6)
        else:
            # One machine instruction per host turn: INC runs, then the
            # session suspends with the routine's RET still to come.
            start_command(6)
            advance_until(lambda: hybrid.machine_instructions == 1)
            suspended = runtime._suspended_execution
            cursor = suspended.cursor
            assert type(cursor) is _MachineCursor and cursor.host_yield is True
            assert hybrid.parked
            assert _stored_cell(runtime, "HR-DONE") == 5
            assert _stored_cell(runtime, "HR-MACHINE-VALUE") == 0
            context = runtime.main_context
            parked_stacks = (context.data.snapshot(), context.returns.snapshot())
            parked_steps = session.semantic_steps_total
            parked_status = server.dispatch("status", {})["machine_execution"]
            assert parked_status["quantum_instructions"] == 1
            assert (parked_status["instructions"], parked_status["transitions"]) == (1, 1)
        assert session.last_acknowledged_display_offer == (offer.offer_id, offer.scope)

        events_before = server.dispatch("status", {})["external_events_applied"]
        for change, expected in (
            ({"generation": generation + 1}, "stale_generation"),
            ({"display_offer_id": offer.offer_id + 1}, "stale_display"),
        ):
            assert server.dispatch("send_control_event", activation | change,
                                   connection_id=CONNECTION) == {
                "status": expected, "accepted_events": 0,
            }
        assert _stored_cell(runtime, "HR-EVENT-COUNT") == 0
        assert server.dispatch("send_control_event", activation, connection_id=CONNECTION) == {
            "status": "progress", "accepted_events": 1,
        }
        if yields:
            # Enhanced input is queued while the routine's cursor is retained.
            # Admission and status must not resume RET, consume the event or
            # touch the guest stacks.
            assert runtime._suspended_execution is suspended
            assert suspended.cursor is cursor and hybrid.parked
            assert hybrid.machine_instructions == 1
            assert session.semantic_steps_total == parked_steps
            assert (context.data.snapshot(), context.returns.snapshot()) == parked_stacks
            assert server.dispatch("status", {})["external_events_applied"] == events_before
            assert _stored_cell(runtime, "HR-EVENT-COUNT") == 0
        advance_until(lambda: _stored_cell(runtime, "HR-EVENT-COUNT") == 1
                      and _stored_cell(runtime, "HR-DONE") == 6)
        assert server.dispatch("status", {})["external_events_applied"] == events_before + 1
        assert _stored_cell(runtime, "HR-MACHINE-VALUE") == 42
        assert hybrid.transitions == 1 and hybrid.machine_instructions == 2
        assert hybrid.machine_segments == (2 if yields else 1)
        assert not hybrid.parked
        adjustment = dict(proof, owner_id=OWNER, owner_generation=GENERATION,
                          control_id=FIELD_ID, modifiers=2,
                          event_kind=int(ControlEventKind.ADJUST),
                          content_revision=FIELD_REVISION, adjustment=-2)
        assert server.dispatch("send_text_event", adjustment, connection_id=CONNECTION) == {
            "status": "progress", "accepted_events": 1,
        }
        advance_until(lambda: _stored_cell(runtime, "HR-EVENT-COUNT") == 2)
        assert server.dispatch("status", {})["external_events_applied"] == events_before + 2
        assert hybrid.machine_cycles == 3 and hybrid.callback_requests == 0
        records = runtime.find("HR-RECORDS").body_address
        assert tuple(runtime.memory.read64(records + offset) for offset in range(0, 128, 8)) == (
            OWNER, GENERATION, TASK_ID, int(ControlEventKind.ACTIVATE), 5,
            offer.scope.model_revision, 0, 0,
            OWNER, GENERATION, FIELD_ID, int(ControlEventKind.ADJUST), 2,
            offer.scope.model_revision, FIELD_REVISION, (1 << 64) - 2,
        )
        assert session.rich_terminal_driver.core.retained_state is scene
        assert controls[FIELD_ID].content.value == 441
        assert session.last_acknowledged_display_offer == (offer.offer_id, offer.scope)
    finally:
        if server is not None:
            server.stop()
        else:
            hybrid.close()
    assert backend.closed and hybrid.closed
    assert machine._thread is not None and not machine._thread.is_alive()
    assert server._display_holder is None
    assert session.rich_terminal_driver is None
    assert runtime._session_owner_token is None
    assert runtime._session_owner_thread is None
