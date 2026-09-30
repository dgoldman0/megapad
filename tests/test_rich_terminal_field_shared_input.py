"""FIELD adjustment preserves physical display proofs across shared input."""

import pytest

from tests import test_rich_terminal_semantic_shared_input as rpc
from tests import test_rich_terminal_semantic_session_input as local
from rich_terminal.driver import DriverStatus
from rich_terminal.retained_wire import ControlEventKind


def _params(**changes):
    return rpc._params(event_kind=int(ControlEventKind.ADJUST),
                       content_revision=7, adjustment=-2, **changes)


def test_adjust_rpc_preserves_exact_signed_request_and_uses_existing_event_route():
    server, session = rpc._server()
    assert server.dispatch("send_text_event", _params(), connection_id=rpc.CONNECTION) == {
        "status": "progress", "accepted_events": 1,
    }
    assert session.events == [(7, 3, 11, 0x15)]
    assert session.tails == [(ControlEventKind.ADJUST, {
        "content_revision": 7, "item_key": 0, "scalar_offset": 0,
        "wheel_x": 0, "wheel_y": 0, "adjustment": -2,
    })]


@pytest.mark.parametrize("changes", [
    {"generation": rpc.GENERATION + 1},
    {"display_offer_id": rpc.DISPLAY_PROOF[0] + 1},
])
def test_adjust_rejects_stale_generation_or_display_before_forwarding(changes):
    server, session = rpc._server()
    result = server.dispatch("send_text_event", _params(**changes), connection_id=rpc.CONNECTION)
    assert result["status"] == ("stale_generation" if "generation" in changes else "stale_display")
    assert result["accepted_events"] == 0
    assert not session.events


def test_adjust_requires_holder_and_exact_acknowledged_offer():
    server, session = rpc._server()
    assert server.dispatch("send_text_event", _params(), connection_id=rpc.CONNECTION + 1) == {
        "status": "stale_display", "accepted_events": 0,
    }
    server._display_ack = None
    assert server.dispatch("send_text_event", _params(), connection_id=rpc.CONNECTION) == {
        "status": "backpressured", "accepted_events": 0,
    }
    assert not session.events


@pytest.mark.parametrize("field,value", [
    ("adjustment", True), ("adjustment", 0), ("adjustment", 1 << 63),
    ("adjustment", -(1 << 63) - 1), ("adjustment", "1"),
    ("content_revision", 0), ("content_revision", True), ("content_revision", 1 << 64),
])
def test_adjust_rpc_rejects_noncanonical_numbers_before_forwarding(field, value):
    server, session = rpc._server()
    params = _params()
    params[field] = value
    with pytest.raises((TypeError, ValueError)):
        server.dispatch("send_text_event", params, connection_id=rpc.CONNECTION)
    assert not session.events


@pytest.mark.parametrize("field", ["item_key", "scalar_offset", "wheel_x", "wheel_y", "model_revision"])
def test_adjust_rpc_rejects_even_zero_extraneous_tail_fields(field):
    server, session = rpc._server()
    with pytest.raises(ValueError, match="fields are not exact"):
        server.dispatch("send_text_event", _params(**{field: 0}), connection_id=rpc.CONNECTION)
    assert not session.events


@pytest.mark.parametrize("field", ["content_revision", "adjustment", "display_scope"])
def test_adjust_rpc_requires_all_authority_and_tail_fields(field):
    server, session = rpc._server()
    params = _params()
    del params[field]
    with pytest.raises(ValueError, match="fields are not exact"):
        server.dispatch("send_text_event", params, connection_id=rpc.CONNECTION)
    assert not session.events


def test_backpressured_adjustment_is_not_rebound_when_display_proof_changes():
    server, session = rpc._server(status=DriverStatus.BACKPRESSURED)
    params = _params()
    assert server.dispatch("send_text_event", params, connection_id=rpc.CONNECTION) == {
        "status": "backpressured", "accepted_events": 0,
    }
    assert len(session.events) == 1
    session.status = DriverStatus.PROGRESS
    session.last_acknowledged_display_offer = (rpc.DISPLAY_PROOF[0] + 1, rpc.SCOPE)
    assert server.dispatch("send_text_event", params, connection_id=rpc.CONNECTION) == {
        "status": "stale_display", "accepted_events": 0,
    }
    assert len(session.events) == 1
    assert session.tails[0][1]["content_revision"] == 7


def test_session_forwards_adjustment_with_exact_acknowledged_model_and_content_revision():
    session, driver, _, scope = local._ready_session()
    assert session.send_control_event(7, 2, 11, event_kind=ControlEventKind.ADJUST,
                                      content_revision=7, adjustment=-2) is DriverStatus.PROGRESS
    assert driver.control_events == [(7, 2, 11, 0, scope.model_revision)]
    assert driver.event_tails == [(ControlEventKind.ADJUST, {
        "content_revision": 7, "item_key": 0, "scalar_offset": 0,
        "wheel_x": 0, "wheel_y": 0, "adjustment": -2,
    })]


def test_session_does_not_forward_adjustment_while_newer_output_awaits_display():
    session, driver, _, scope = local._ready_session()
    session._display_cadence.pending_revision = scope.model_revision + 1
    assert session.send_control_event(7, 2, 11, event_kind=ControlEventKind.ADJUST,
                                      content_revision=7, adjustment=-2) is DriverStatus.BACKPRESSURED
    assert not driver.control_events


def test_field_activate_preserves_compact_rpc_shape_and_old_zero_tail():
    server, session = rpc._server()
    assert server.dispatch("send_control_event", rpc._params(), connection_id=rpc.CONNECTION) == {
        "status": "progress", "accepted_events": 1,
    }
    assert session.tails == [(ControlEventKind.ACTIVATE, {
        "content_revision": 0, "item_key": 0, "scalar_offset": 0,
        "wheel_x": 0, "wheel_y": 0,
    })]
    with pytest.raises(ValueError, match="fields are not exact"):
        server.dispatch("send_control_event", _params(), connection_id=rpc.CONNECTION)
