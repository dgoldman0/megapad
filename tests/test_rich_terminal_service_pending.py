"""PT-SERVICE-PENDING? reports exactly the work PT-SERVICE can do without input.

A caller may sleep until input only when it is false.  Each case starts from
a real PT-INIT session, sets the fields the matching PT-SERVICE step reads,
and checks the answer in both execution backends.
"""

from __future__ import annotations

import re

import pytest

from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    ONE_CORE_UART_LOCK_SHIMS,
    RICH_TERMINAL_SOURCE,
    SIMULATOR_SOURCE_MAX_STEPS,
    _rich_terminal_module_source,
)

MASK64 = (1 << 64) - 1
BUFFER_BYTES = 65536

# Session field offsets, read from the accessor definitions themselves.
OFFSETS = {
    match[1]: int(match[2] or 0)
    for match in re.finditer(
        r"(?m)^: _PT\.S\.(\S+)\s+\( s -- a \)\s*(\d*)",
        RICH_TERMINAL_SOURCE.read_text(),
    )
}


class Session:
    def __init__(self, backend: str) -> None:
        self.runtime = _load_exceptions(MegaForthRuntime(execution_backend=backend))
        self.runtime.evaluate(
            ONE_CORE_UART_LOCK_SHIMS + _rich_terminal_module_source(),
            source_name="one-core-uart-lock-shims+rich-terminal.f:complete",
            step_budget=SIMULATOR_SOURCE_MAX_STEPS,
        )
        self.serial = 0
        rx = self.allocate(BUFFER_BYTES)
        tx = self.allocate(BUFFER_BYTES)
        event = self.allocate(self.value("PT-EVENT-SIZE"))
        self.address = self.allocate(self.value("PT-SESSION-SIZE"))
        status = self.results(
            "PT-INIT", rx, BUFFER_BYTES, tx, BUFFER_BYTES,
            event, self.value("PT-EVENT-SIZE"), self.address,
        )
        assert status == (self.value("PT-S-OK"),)

    def allocate(self, size: int) -> int:
        self.serial += 1
        word = self.runtime.define_created(
            f"PENDING-BUFFER-{self.serial}", initial_body=bytes(size + 7),
        )
        return (word.body_address + 7) & -8

    def results(self, name: str, *inputs: int) -> tuple[int, ...]:
        stack = self.runtime.main_context.data
        for value in inputs:
            stack.push(value & MASK64)
        self.runtime.execute(name, step_budget=1_000_000)
        result = stack.snapshot()
        while stack.depth():
            stack.pop()
        assert self.runtime.main_context.returns.snapshot() == ()
        return result

    def value(self, name: str) -> int:
        (result,) = self.results(name)
        return result

    def set(self, field: str, value: int) -> None:
        self.runtime.memory.write64(self.address + OFFSETS[field], value & MASK64)

    def get(self, field: str) -> int:
        return self.runtime.memory.read64(self.address + OFFSETS[field])

    def state(self, name: str) -> None:
        self.set("STATE", self.value(name))

    def pending(self, session: int | None = None) -> bool:
        address = self.address if session is None else session
        (flag,) = self.results("PT-SERVICE-PENDING?", address)
        assert flag in (0, MASK64)
        return flag == MASK64

    def buffer_frame(self, *, length: int, present: int) -> None:
        """Place a header declaring LENGTH payload bytes, with PRESENT bytes
        of the frame received so far."""

        header = bytearray(self.value("_PT-HDR"))
        header[12:16] = length.to_bytes(4, "little")
        self.runtime.memory.write_bytes(self.get("RX-A"), bytes(header))
        self.set("BIN-U", present)


@pytest.fixture(params=["python", "native"])
def session(request) -> Session:
    return Session(request.param)


def test_a_steady_active_or_resyncing_session_waits_only_for_input(session):
    session.state("PT-ST-ACTIVE")
    assert not session.pending()
    session.state("PT-ST-RESYNCING")
    assert not session.pending()


@pytest.mark.parametrize("state", ["PT-ST-ANSI", "PT-ST-LOST"])
def test_ansi_and_lost_sessions_have_no_service_work(session, state):
    session.state(state)
    session.set("EVENT-PENDING", 1)
    assert not session.pending()


@pytest.mark.parametrize("state", ["PT-ST-PROBING", "PT-ST-OPENING", "PT-ST-CLOSING"])
def test_handshakes_and_closes_advance_on_their_own_deadlines(session, state):
    session.state(state)
    assert session.pending()


def test_a_missing_or_unsigned_session_is_not_pending(session):
    assert not session.pending(0)
    assert not session.pending(session.allocate(session.value("PT-SESSION-SIZE")))


@pytest.mark.parametrize("field", ["CLOSE-PENDING?", "EVENT-PENDING", "COMPLETE?"])
def test_close_settlement_events_and_completions_are_pending(session, field):
    session.state("PT-ST-ACTIVE")
    session.set(field, 1)
    assert session.pending()


def test_unread_uart_input_is_pending(session):
    session.state("PT-ST-ACTIVE")
    session.runtime.inject_uart_input(b"\xa5")
    assert session.pending()


def test_only_a_complete_or_rejectable_buffered_frame_is_pending(session):
    session.state("PT-ST-ACTIVE")
    session.set("CLIENT-MAX-PAY", 4096)
    header = session.value("_PT-HDR")
    session.buffer_frame(length=8, present=header - 1)
    assert not session.pending()
    session.buffer_frame(length=8, present=header + 7)
    assert not session.pending()
    session.buffer_frame(length=8, present=header + 8)
    assert session.pending()
    # Lengths the next service rejects as soon as the header is complete.
    session.buffer_frame(length=4097, present=header)
    assert session.pending()
    session.set("CLIENT-MAX-PAY", MASK64)
    session.buffer_frame(length=BUFFER_BYTES, present=header)
    assert session.pending()


def test_sequence_exhaustion_starts_a_close(session):
    session.state("PT-ST-ACTIVE")
    session.set("TX-SEQ", MASK64 - 1)
    assert session.pending()


def test_owed_credit_is_pending_only_when_service_may_send_it(session):
    session.state("PT-ST-ACTIVE")
    session.set("CREDIT-DIRTY?", 1)
    assert session.pending()
    session.set("TX-OPEN?", 1)
    assert not session.pending()
    session.set("TX-OPEN?", 0)
    session.set("SPAN-REMAIN", 5)
    assert not session.pending()
    session.set("SPAN-REMAIN", 0)
    # A held reset defers credit; this one is itself waiting for a result.
    session.set("RESET-PENDING?", 1)
    session.set("AWAIT?", 1)
    assert not session.pending()


@pytest.mark.parametrize("blocker", [None, "TX-OPEN?", "AWAIT?", "LIFE-AWAIT?"])
def test_a_held_reset_is_pending_once_nothing_blocks_it(session, blocker):
    session.state("PT-ST-ACTIVE")
    session.set("RESET-PENDING?", 1)
    if blocker is None:
        assert session.pending()
    else:
        session.set(blocker, 1)
        assert not session.pending()


def test_retained_activation_is_pending_once_covering_credit_arrived(session):
    session.state("PT-ST-ACTIVE")
    session.set("RET-STATE", session.value("_PT-RD-WAIT-CREDIT"))
    session.set("RET-WATERMARK", 500)
    session.set("PEER-GRANT", 499)
    assert not session.pending()
    session.set("PEER-GRANT", 500)
    assert session.pending()
    session.set("TX-OPEN?", 1)
    assert not session.pending()


def test_retained_discovery_is_pending_unless_it_waits_for_peer_credit(session):
    session.state("PT-ST-ACTIVE")
    session.set("RET-ENABLED?", 1)
    session.set("RET-STATE", session.value("_PT-RD-SNAPSHOT"))
    session.set("LOCAL-GRANT", 4096)
    session.set("PEER-GRANT", 1000)
    session.set("PEER-SENT", 1000 - 48)
    assert session.pending()
    session.set("PEER-SENT", 1000 - 47)
    assert not session.pending()
    # A local grant too small for the reply ends discovery: progress.
    session.set("LOCAL-GRANT", session.value("_PT-RET-REPLY-BYTES") - 1)
    assert session.pending()
    # Discovery runs only from ACTIVE after the initial snapshot.
    session.set("LOCAL-GRANT", 4096)
    session.set("PEER-SENT", 0)
    session.set("SNAPSHOT?", 1)
    assert not session.pending()
    session.set("SNAPSHOT?", 0)
    session.state("PT-ST-RESYNCING")
    assert not session.pending()
