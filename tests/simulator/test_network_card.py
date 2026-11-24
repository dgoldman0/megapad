"""The simulator's network card: the BIOS's four network words over a host
frame backend, with the emulator card's status bits (simulator/network.py)."""

from __future__ import annotations

import pytest

from nic_backends import NIC_MAX_FRAME, NICBackend
from simulator.errors import ExecutionError
from simulator.network import (
    DEFAULT_MAC,
    RECEIVE_LIMIT,
    STATUS_ERROR,
    STATUS_LINK,
    STATUS_PRESENT,
    STATUS_RECEIVED,
    NetworkCard,
)
from simulator.runtime import MegaForthRuntime


class RecordingBackend(NICBackend):
    """A backend that keeps what the guest sends and refuses on request."""

    def __init__(self, *, accepts: bool = True):
        super().__init__()
        self.sent: list[bytes] = []
        self.accepts = accepts
        self.running = False

    def send(self, frame: bytes) -> bool:
        self.sent.append(bytes(frame))
        return self.accepts

    def start(self):
        self.running = True

    def stop(self):
        self.running = False

    @property
    def link_up(self) -> bool:
        return self.running


def _machine(backend: NICBackend | None = None):
    runtime = MegaForthRuntime()
    card = NetworkCard(backend or RecordingBackend())
    runtime.attach_network(card)
    card.start()
    return runtime, card


def _word(runtime: MegaForthRuntime, name: str, *inputs: int) -> tuple[int, ...]:
    context = runtime.new_context()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name, context=context)
    return context.data.snapshot()


def _buffer(runtime: MegaForthRuntime, name: bytes, content: bytes) -> int:
    return runtime.define_created(name, initial_body=content).body_address


def test_status_reports_a_present_card_with_its_link() -> None:
    runtime, card = _machine()
    assert _word(runtime, "NET-STATUS") == (STATUS_PRESENT | STATUS_LINK,)
    card.stop()
    assert _word(runtime, "NET-STATUS") == (STATUS_PRESENT,)


def test_mac_word_reports_the_cards_address() -> None:
    runtime, _ = _machine()
    (address,) = _word(runtime, "NET-MAC@")
    assert runtime.memory.read_bytes(address, 6) == DEFAULT_MAC


def test_send_takes_the_frame_from_guest_memory() -> None:
    backend = RecordingBackend()
    runtime, _ = _machine(backend)
    frame = bytes(range(60))
    address = _buffer(runtime, b"FRAME", frame)
    assert _word(runtime, "NET-SEND", address, len(frame)) == ()
    assert backend.sent == [frame]
    assert _word(runtime, "NET-STATUS") == (STATUS_PRESENT | STATUS_LINK,)


@pytest.mark.parametrize("length", [0, NIC_MAX_FRAME + 1])
def test_a_frame_the_card_cannot_carry_sets_the_error_bit(length) -> None:
    backend = RecordingBackend()
    runtime, _ = _machine(backend)
    address = _buffer(runtime, b"FRAME", bytes(NIC_MAX_FRAME + 1))
    _word(runtime, "NET-SEND", address, length)
    assert backend.sent == []
    assert _word(runtime, "NET-STATUS") == (STATUS_PRESENT | STATUS_LINK | STATUS_ERROR,)


def test_a_span_outside_ordinary_memory_sets_the_error_bit() -> None:
    backend = RecordingBackend()
    runtime, _ = _machine(backend)
    _word(runtime, "NET-SEND", 0, 60)
    assert backend.sent == []
    assert _word(runtime, "NET-STATUS")[0] & STATUS_ERROR


def test_a_frame_the_backend_refuses_sets_the_error_bit() -> None:
    runtime, _ = _machine(RecordingBackend(accepts=False))
    address = _buffer(runtime, b"FRAME", bytes(60))
    _word(runtime, "NET-SEND", address, 60)
    assert _word(runtime, "NET-STATUS")[0] & STATUS_ERROR


def test_receive_places_each_waiting_frame_in_turn() -> None:
    backend = RecordingBackend()
    runtime, _ = _machine(backend)
    destination = _buffer(runtime, b"INBOX", bytes(NIC_MAX_FRAME))
    assert _word(runtime, "NET-RECV", destination) == (0,)

    backend.on_rx_frame(b"first frame")
    backend.on_rx_frame(b"second")
    assert _word(runtime, "NET-STATUS") == (STATUS_PRESENT | STATUS_LINK | STATUS_RECEIVED,)
    assert _word(runtime, "NET-RECV", destination) == (11,)
    assert runtime.memory.read_bytes(destination, 11) == b"first frame"
    assert _word(runtime, "NET-RECV", destination) == (6,)
    assert runtime.memory.read_bytes(destination, 6) == b"second"
    assert _word(runtime, "NET-RECV", destination) == (0,)
    assert _word(runtime, "NET-STATUS") == (STATUS_PRESENT | STATUS_LINK,)


def test_a_frame_past_the_cards_limit_is_lost_and_sets_the_error_bit() -> None:
    backend = RecordingBackend()
    runtime, _ = _machine(backend)
    for number in range(RECEIVE_LIMIT + 1):
        backend.on_rx_frame(bytes([number]) * 20)
    destination = _buffer(runtime, b"INBOX", bytes(NIC_MAX_FRAME))
    received = []
    while (length := _word(runtime, "NET-RECV", destination)[0]):
        received.append(runtime.memory.read_bytes(destination, length)[0])
    assert received == list(range(RECEIVE_LIMIT))
    assert _word(runtime, "NET-STATUS")[0] & STATUS_ERROR


def test_a_machine_takes_one_card() -> None:
    runtime, _ = _machine()
    with pytest.raises(ExecutionError, match="already has a network card"):
        runtime.attach_network(NetworkCard(RecordingBackend()))


def test_the_default_address_is_the_emulators() -> None:
    import inspect

    from emulator.devices import NetworkDevice

    default = inspect.signature(NetworkDevice.__init__).parameters["mac"].default
    assert default == DEFAULT_MAC
