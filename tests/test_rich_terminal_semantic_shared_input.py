"""Focused shared-lease RPC checks for semantic control activation."""

from __future__ import annotations

import pytest

from rich_terminal import DriverStatus
from rich_terminal.retained_view import DisplayScope
from shared_session import SessionServer, SharedMachine, display_scope_to_wire


GENERATION = 4
CONNECTION = 9
SCOPE = DisplayScope(1, 2, 0, 17, 3, 16, 17)
DISPLAY_PROOF = (6, SCOPE)


class _Session:
    retained_display_required = True
    last_acknowledged_display_offer = DISPLAY_PROOF
    rich_terminal_failure = None

    def __init__(self, status: DriverStatus = DriverStatus.PROGRESS) -> None:
        self.status = status
        self.events: list[tuple[int, int, int, int]] = []

    def send_control_event(
        self,
        owner_id: int,
        owner_generation: int,
        control_id: int,
        *,
        modifiers: int = 0,
    ) -> DriverStatus:
        self.events.append(
            (owner_id, owner_generation, control_id, modifiers)
        )
        return self.status


def _server(
    *,
    status: DriverStatus = DriverStatus.PROGRESS,
) -> tuple[SessionServer, _Session]:
    session = _Session(status)
    machine = SharedMachine(session)
    machine._reset_generation = GENERATION
    server = SessionServer(machine, "/tmp/unused-rich-terminal-semantic.sock")
    server._display_holder = CONNECTION
    server._display_delivered = DISPLAY_PROOF
    server._display_ack = DISPLAY_PROOF
    return server, session


def _params(**changes) -> dict:
    params = {
        "generation": GENERATION,
        "display_offer_id": DISPLAY_PROOF[0],
        "display_scope": display_scope_to_wire(SCOPE),
        "owner_id": 7,
        "owner_generation": 3,
        "control_id": 11,
        "modifiers": 0x15,
    }
    params.update(changes)
    return params


def test_shared_rpc_forwards_only_qualified_identity_and_modifiers():
    server, session = _server()

    result = server.dispatch(
        "send_control_event",
        _params(),
        connection_id=CONNECTION,
    )

    assert result == {"status": "progress", "accepted_events": 1}
    assert session.events == [(7, 3, 11, 0x15)]


def test_shared_rpc_rejects_stale_generation_display_and_nonholder():
    server, session = _server()

    stale_generation = server.dispatch(
        "send_control_event",
        _params(generation=GENERATION + 1),
        connection_id=CONNECTION,
    )
    wrong_offer = server.dispatch(
        "send_control_event",
        _params(display_offer_id=DISPLAY_PROOF[0] + 1),
        connection_id=CONNECTION,
    )
    nonholder = server.dispatch(
        "send_control_event",
        _params(),
        connection_id=CONNECTION + 1,
    )

    assert stale_generation == {
        "status": "stale_generation",
        "accepted_events": 0,
    }
    assert wrong_offer == {"status": "stale_display", "accepted_events": 0}
    assert nonholder == {"status": "stale_display", "accepted_events": 0}
    assert session.events == []


def test_shared_rpc_requires_server_and_session_to_hold_the_same_ack():
    server, session = _server()
    server._display_ack = None

    missing = server.dispatch(
        "send_control_event",
        _params(),
        connection_id=CONNECTION,
    )
    assert missing == {"status": "backpressured", "accepted_events": 0}

    server._display_ack = DISPLAY_PROOF
    session.last_acknowledged_display_offer = (DISPLAY_PROOF[0] + 1, SCOPE)
    mismatched = server.dispatch(
        "send_control_event",
        _params(),
        connection_id=CONNECTION,
    )
    assert mismatched == {"status": "stale_display", "accepted_events": 0}
    assert session.events == []


def test_shared_rpc_preserves_driver_backpressure_without_accepting_event():
    server, session = _server(status=DriverStatus.BACKPRESSURED)

    result = server.dispatch(
        "send_control_event",
        _params(),
        connection_id=CONNECTION,
    )

    assert result == {"status": "backpressured", "accepted_events": 0}
    assert session.events == [(7, 3, 11, 0x15)]


@pytest.mark.parametrize(
    ("status", "paused"),
    (
        (DriverStatus.INVALID, False),
        (DriverStatus.STALE, True),
        (DriverStatus.FAILED, True),
    ),
)
def test_shared_rpc_preserves_terminal_status_and_lifetime_effects(status, paused):
    server, session = _server(status=status)

    result = server.dispatch(
        "send_control_event",
        _params(),
        connection_id=CONNECTION,
    )

    assert result == {"status": status.value, "accepted_events": 0}
    assert server.machine.paused is paused
    assert (server.machine.last_error is not None) is paused
    assert session.events == [(7, 3, 11, 0x15)]


@pytest.mark.parametrize(
    "changes",
    (
        {"owner_id": True},
        {"owner_id": 0},
        {"owner_generation": 1 << 64},
        {"control_id": 0},
        {"modifiers": 0x40},
    ),
)
def test_shared_rpc_rejects_noncanonical_control_scalars(changes):
    server, session = _server()

    with pytest.raises((TypeError, ValueError)):
        server.dispatch(
            "send_control_event",
            _params(**changes),
            connection_id=CONNECTION,
        )
    assert session.events == []


@pytest.mark.parametrize(
    ("mutate", "match"),
    (
        (lambda params: params.pop("modifiers"), "fields are not exact"),
        (lambda params: params.update({"model_revision": 17}), "fields are not exact"),
        (lambda params: params.update({"event_kind": 1}), "fields are not exact"),
        (lambda params: params.pop("display_scope"), "fields are not exact"),
    ),
)
def test_shared_rpc_has_one_exact_authority_shape(mutate, match):
    server, session = _server()
    params = _params()
    mutate(params)

    with pytest.raises(ValueError, match=match):
        server.dispatch(
            "send_control_event",
            params,
            connection_id=CONNECTION,
        )
    assert session.events == []
