"""Hosted semantic state for the shared 256-coefficient NTT engine.

The service models terminal polynomial values, register persistence, command
status, and the BIOS byte-transfer order.  It does not emulate the physical
MMIO aperture, command latency, bus arbitration, or hardware timing.
"""

from __future__ import annotations

from shared.cells import u64
from shared.ntt import (
    NTT_COEFFICIENT_BYTES,
    NTT_DEFAULT_MODULUS,
    NTT_SIZE,
    NTTRoots,
    find_ntt_roots,
    ntt_forward,
    ntt_inverse,
    ntt_pointwise_add,
    ntt_pointwise_multiply,
)
from simulator.memory import SparseAddressSpace


NTT_STATUS_IDLE = 0
NTT_STATUS_BUSY = 1
NTT_STATUS_DONE = 2


class HostedNTTService:
    """One runtime-local NTT device shared by all semantic callers."""

    def __init__(self) -> None:
        self._modulus = NTT_DEFAULT_MODULUS
        self._index = 0
        self._poly_a = [0] * NTT_SIZE
        self._poly_b = [0] * NTT_SIZE
        self._result = [0] * NTT_SIZE
        self._busy = False
        self._done = False
        self._load_a_stage = bytearray(NTT_COEFFICIENT_BYTES)
        self._load_b_stage = bytearray(NTT_COEFFICIENT_BYTES)
        self._roots = find_ntt_roots(self._modulus)

    @property
    def modulus(self) -> int:
        """Return the retained uint64 modulus."""

        return self._modulus

    @property
    def index(self) -> int:
        """Return the retained coefficient index register."""

        return self._index

    @property
    def status(self) -> int:
        """Return the raw idle/busy/done status bits."""

        return (int(self._done) << 1) | int(self._busy)

    @property
    def roots(self) -> NTTRoots | None:
        """Return the current device-selected root tuple, if any."""

        return self._roots

    def polynomial_a(self) -> tuple[int, ...]:
        """Return a diagnostic snapshot of input buffer A."""

        return tuple(self._poly_a)

    def polynomial_b(self) -> tuple[int, ...]:
        """Return a diagnostic snapshot of input buffer B."""

        return tuple(self._poly_b)

    def result(self) -> tuple[int, ...]:
        """Return a diagnostic snapshot of the result buffer."""

        return tuple(self._result)

    def load_stage(self, selector: int) -> bytes:
        """Return the selected partial four-byte input staging register."""

        selector = self._cell(selector, label="NTT buffer selector")
        stage = self._load_a_stage if selector == 0 else self._load_b_stage
        return bytes(stage)

    def set_modulus(self, value: int) -> None:
        """Replace Q and recompute roots without changing buffers or status."""

        self._modulus = self._cell(value, label="NTT modulus")
        self._roots = find_ntt_roots(self._modulus)

    def set_index(self, value: int) -> None:
        """Replace the raw 16-bit coefficient index register."""

        self._index = self._cell(value, label="NTT index") & 0xFFFF

    def load(
        self,
        address: int,
        selector: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Load 256 uint32 coefficients with BIOS byte/fault ordering."""

        address = self._cell(address, label="NTT source address")
        selector = self._cell(selector, label="NTT buffer selector")
        memory = self._memory(memory)
        stage = self._load_a_stage if selector == 0 else self._load_b_stage
        polynomial = self._poly_a if selector == 0 else self._poly_b
        self._index = 0
        for coefficient in range(NTT_SIZE):
            source = u64(address + coefficient * NTT_COEFFICIENT_BYTES)
            for byte_index in range(NTT_COEFFICIENT_BYTES):
                stage[byte_index] = memory.read8(u64(source + byte_index))
            index = self._index % NTT_SIZE
            polynomial[index] = int.from_bytes(stage, "little") % self._modulus
            self._index = (self._index + 1) % NTT_SIZE

    def store(self, address: int, memory: SparseAddressSpace) -> None:
        """Store 256 result uint32s with device-read-before-write ordering."""

        address = self._cell(address, label="NTT destination address")
        memory = self._memory(memory)
        self._index = 0
        for coefficient in range(NTT_SIZE):
            destination = u64(address + coefficient * NTT_COEFFICIENT_BYTES)
            value = self._result[self._index % NTT_SIZE]
            for byte_index in range(NTT_COEFFICIENT_BYTES):
                byte = (value >> (byte_index * 8)) & 0xFF
                if byte_index == NTT_COEFFICIENT_BYTES - 1:
                    self._index = (self._index + 1) % NTT_SIZE
                memory.write8(u64(destination + byte_index), byte)

    def forward(self) -> None:
        """Synchronously transform polynomial A into the result buffer."""

        self._execute("forward")

    def inverse(self) -> None:
        """Synchronously inverse-transform polynomial A into result."""

        self._execute("inverse")

    def pointwise_multiply(self) -> None:
        """Synchronously multiply A and B coefficient-by-coefficient."""

        self._execute("multiply")

    def pointwise_add(self) -> None:
        """Synchronously add A and B coefficient-by-coefficient."""

        self._execute("add")

    def reset(self) -> None:
        """Restore the hosted device's construction state."""

        self._modulus = NTT_DEFAULT_MODULUS
        self._index = 0
        self._poly_a[:] = [0] * NTT_SIZE
        self._poly_b[:] = [0] * NTT_SIZE
        self._result[:] = [0] * NTT_SIZE
        self._busy = False
        self._done = False
        self._load_a_stage[:] = bytes(NTT_COEFFICIENT_BYTES)
        self._load_b_stage[:] = bytes(NTT_COEFFICIENT_BYTES)
        self._roots = find_ntt_roots(self._modulus)

    def _execute(self, operation: str) -> None:
        if self._busy:
            return
        self._busy = True
        self._done = False
        if self._roots is None:
            self._busy = False
            self._done = True
            return
        if operation == "forward":
            result = ntt_forward(
                self._poly_a,
                self._modulus,
                roots=self._roots,
            )
        elif operation == "inverse":
            result = ntt_inverse(
                self._poly_a,
                self._modulus,
                roots=self._roots,
            )
        elif operation == "multiply":
            result = ntt_pointwise_multiply(
                self._poly_a,
                self._poly_b,
                self._modulus,
            )
        elif operation == "add":
            result = ntt_pointwise_add(
                self._poly_a,
                self._poly_b,
                self._modulus,
            )
        else:
            raise AssertionError(f"unknown hosted NTT operation {operation!r}")
        self._result[:] = result
        self._busy = False
        self._done = True

    @staticmethod
    def _memory(memory: SparseAddressSpace) -> SparseAddressSpace:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("NTT memory must be a SparseAddressSpace")
        return memory

    @staticmethod
    def _cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer")
        return u64(value)


__all__ = [
    "HostedNTTService",
    "NTT_STATUS_BUSY",
    "NTT_STATUS_DONE",
    "NTT_STATUS_IDLE",
]
