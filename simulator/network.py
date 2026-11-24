"""The simulator's network card.

The simulator runs the BIOS's four network words as primitives
(``simulator/core_words.py``).  With no card attached they behave as a
machine without a network: ``NET-STATUS`` reports nothing present,
``NET-SEND`` sends nothing and ``NET-RECV`` receives nothing.  A server
started with ``--nic`` attaches a ``NetworkCard`` over one of
``nic_backends``' host backends, and the words then behave as the
emulator's card does (``emulator/accel/mp64_nic.h``), through the same
status bits:

- bit 7: a card is present;
- bit 1: a received frame waits;
- bit 2: the link is up;
- bit 3: an error occurred, which stays set: a frame the card could not
  carry or place, or a frame lost because 64 were already waiting.

The emulator's card completes each frame's DMA at once in this case, so
bit 0 (sending) and bit 4 (receiving) never show here.  A frame's bytes
move between guest memory and the host only through ordinary memory the
caller owns; any other span sets the error bit, where the emulator's card
would read zeros or drop the bytes.

This module loads only when a server attaches a card, so a change here
reruns only the tests that use one.
"""

from __future__ import annotations

import threading
from collections import deque

from nic_backends import NIC_MAX_FRAME, NICBackend

# The emulator's card has the same default (emulator/devices.py), so a
# guest finds the same address on either machine.
DEFAULT_MAC = b"\x02\x4D\x50\x36\x34\x00"

STATUS_PRESENT = 0x80
STATUS_RECEIVED = 0x02
STATUS_LINK = 0x04
STATUS_ERROR = 0x08

# Frames the card holds for the guest, as many as the emulator's card holds.
RECEIVE_LIMIT = 64

_SPAN_OK = 0


class NetworkCard:
    """One network card over BACKEND, a host frame transport, with MAC as
    its address."""

    def __init__(self, backend: NICBackend, mac: bytes = DEFAULT_MAC):
        if len(mac) != 6:
            raise ValueError("a card's address is six bytes")
        self.backend = backend
        self.mac = bytes(mac)
        self.error = False
        self._received: deque[bytes] = deque()
        self._lock = threading.Lock()
        backend.on_rx_frame = self._arrived

    def start(self) -> None:
        self.backend.start()

    def stop(self) -> None:
        self.backend.stop()

    def _arrived(self, frame: bytes) -> None:
        """A frame for the guest, from the backend's thread."""

        with self._lock:
            if 0 < len(frame) <= NIC_MAX_FRAME and len(self._received) < RECEIVE_LIMIT:
                self._received.append(bytes(frame))
            else:
                self.error = True

    def status(self) -> int:
        with self._lock:
            waiting = bool(self._received)
            error = self.error
        return (STATUS_PRESENT
                | (STATUS_RECEIVED if waiting else 0)
                | (STATUS_LINK if self.backend.link_up else 0)
                | (STATUS_ERROR if error else 0))

    def send_from(self, runtime, context, address: int, length: int) -> None:
        """NET-SEND: send the LENGTH bytes at ADDRESS as one frame."""

        if not 0 < length <= NIC_MAX_FRAME or \
                runtime.caller_span_status(context, address, length) != _SPAN_OK:
            self.error = True
            return
        if not self.backend.send(runtime.memory.read_bytes(address, length)):
            self.error = True

    def receive_into(self, runtime, context, address: int) -> int:
        """NET-RECV: place the next waiting frame at ADDRESS and return its
        length, or return 0 when none waits."""

        with self._lock:
            if not self._received:
                return 0
            frame = self._received.popleft()
        if runtime.caller_span_status(context, address, len(frame)) != _SPAN_OK:
            self.error = True
            return 0
        runtime.memory.write_bytes(address, frame)
        return len(frame)
