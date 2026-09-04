"""Hosted semantic state for MegaPad's executable ML-KEM device.

The service models retained buffers, selector/index state, synchronous command
results, and BIOS byte-transfer order.  It does not emulate the physical MMIO
aperture, RTL stub, latency, bus arbitration, or requester interleaving.
"""

from __future__ import annotations

from shared.cells import u64
from shared.mlkem import (
    MLKEM512_CIPHERTEXT_BYTES,
    MLKEM512_DECAPSULATION_KEY_BYTES,
    MLKEM512_ENCAPSULATION_KEY_BYTES,
    MLKEM512_KEYGEN_SEED_BYTES,
    MLKEM512_SHARED_SECRET_BYTES,
    mlkem512_decapsulate,
    mlkem512_encapsulate,
    mlkem512_keygen,
)
from simulator.memory import SparseAddressSpace


KEM_BUFFER_SEED = 0
KEM_BUFFER_PUBLIC_KEY = 1
KEM_BUFFER_SECRET_KEY = 2
KEM_BUFFER_CIPHERTEXT = 3
KEM_BUFFER_SHARED_SECRET = 4
KEM_BUFFER_SIZES = (
    MLKEM512_KEYGEN_SEED_BYTES,
    MLKEM512_ENCAPSULATION_KEY_BYTES,
    MLKEM512_DECAPSULATION_KEY_BYTES,
    MLKEM512_CIPHERTEXT_BYTES,
    MLKEM512_SHARED_SECRET_BYTES,
)

KEM_STATUS_IDLE = 0
KEM_STATUS_BUSY = 1
KEM_STATUS_DONE = 2


class HostedKEMService:
    """One runtime-global KEM device shared by all semantic callers."""

    def __init__(self) -> None:
        self._status = KEM_STATUS_IDLE
        self._selector = KEM_BUFFER_SEED
        self._index = 0
        self._buffers = [bytearray(size) for size in KEM_BUFFER_SIZES]

    @property
    def status(self) -> int:
        """Return the retained raw status byte."""

        return self._status

    @property
    def selector(self) -> int:
        """Return the selected buffer ID."""

        return self._selector

    @property
    def index(self) -> int:
        """Return the selected buffer's retained byte index."""

        return self._index

    @property
    def selected_size(self) -> int:
        """Return the selected buffer's fixed capacity."""

        return KEM_BUFFER_SIZES[self._selector]

    def buffer(self, selector: int) -> bytes:
        """Return one diagnostic buffer snapshot without changing selection."""

        if isinstance(selector, bool) or not isinstance(selector, int):
            raise TypeError("KEM buffer selector must be an integer")
        if not 0 <= selector < len(KEM_BUFFER_SIZES):
            raise ValueError("KEM buffer selector must be 0 through 4")
        return bytes(self._buffers[selector])

    def select(self, value: int) -> None:
        """Apply the executable low-byte clamp and reset the byte index."""

        value = self._cell(value, label="KEM buffer selector") & 0xFF
        self._selector = min(value, KEM_BUFFER_SHARED_SECRET)
        self._index = 0

    def write_data(self, value: int) -> None:
        """Write one DIN byte, pinning the index when the buffer is full."""

        value = self._cell(value, label="KEM data byte") & 0xFF
        buffer = self._buffers[self._selector]
        if self._index < len(buffer):
            buffer[self._index] = value
            self._index += 1

    def read_data(self) -> int:
        """Read one DOUT byte, returning zero without advancing past full."""

        buffer = self._buffers[self._selector]
        if self._index >= len(buffer):
            return 0
        value = buffer[self._index]
        self._index += 1
        return value

    def load(
        self,
        address: int,
        count: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Transfer caller bytes to DIN in exact BIOS read/write order."""

        address = self._cell(address, label="KEM source address")
        count = self._cell(count, label="KEM load count")
        memory = self._memory(memory)
        for offset in range(count):
            value = memory.read8(u64(address + offset))
            self.write_data(value)

    def store(
        self,
        address: int,
        count: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Transfer DOUT to caller bytes with read-before-write ordering."""

        address = self._cell(address, label="KEM destination address")
        count = self._cell(count, label="KEM store count")
        memory = self._memory(memory)
        for offset in range(count):
            value = self.read_data()
            memory.write8(u64(address + offset), value)

    def keygen(self) -> None:
        """Synchronously replace PK and SK from the retained 64-byte seed."""

        public_key, secret_key = mlkem512_keygen(
            bytes(self._buffers[KEM_BUFFER_SEED])
        )
        self._buffers[KEM_BUFFER_PUBLIC_KEY][:] = public_key
        self._buffers[KEM_BUFFER_SECRET_KEY][:] = secret_key
        self._status = KEM_STATUS_DONE

    def encapsulate(self) -> None:
        """Synchronously replace CT and SS from retained PK and coin bytes."""

        ciphertext, shared_secret = mlkem512_encapsulate(
            bytes(self._buffers[KEM_BUFFER_PUBLIC_KEY]),
            bytes(self._buffers[KEM_BUFFER_SEED][:32]),
        )
        self._buffers[KEM_BUFFER_CIPHERTEXT][:] = ciphertext
        self._buffers[KEM_BUFFER_SHARED_SECRET][:] = shared_secret
        self._status = KEM_STATUS_DONE

    def decapsulate(self) -> None:
        """Synchronously replace SS from retained CT and SK."""

        shared_secret = mlkem512_decapsulate(
            bytes(self._buffers[KEM_BUFFER_CIPHERTEXT]),
            bytes(self._buffers[KEM_BUFFER_SECRET_KEY]),
        )
        self._buffers[KEM_BUFFER_SHARED_SECRET][:] = shared_secret
        self._status = KEM_STATUS_DONE

    def reset(self) -> None:
        """Restore construction state, including zeroing every KEM buffer."""

        self._status = KEM_STATUS_IDLE
        self._selector = KEM_BUFFER_SEED
        self._index = 0
        for buffer in self._buffers:
            buffer[:] = bytes(len(buffer))

    @staticmethod
    def _memory(memory: SparseAddressSpace) -> SparseAddressSpace:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("KEM memory must be a SparseAddressSpace")
        return memory

    @staticmethod
    def _cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer")
        return u64(value)


__all__ = [
    "HostedKEMService",
    "KEM_BUFFER_CIPHERTEXT",
    "KEM_BUFFER_PUBLIC_KEY",
    "KEM_BUFFER_SECRET_KEY",
    "KEM_BUFFER_SEED",
    "KEM_BUFFER_SHARED_SECRET",
    "KEM_BUFFER_SIZES",
    "KEM_STATUS_BUSY",
    "KEM_STATUS_DONE",
    "KEM_STATUS_IDLE",
]
