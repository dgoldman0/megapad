"""Hosted per-core Field-ALU state needed by semantic BIOS words.

The initial slice admits the raw X25519 path over the architectural ACC0-ACC3
and TSRC0 state.  It preserves guest-visible values and sequential memory
effects without emulating EXT.CRYPTO encodings, cycles, stalls, or the RTL
datapath.  Later field words extend this same state rather than creating a
parallel X25519-only service.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from shared.cells import MASK64, u64
from shared.x25519 import X25519_BYTES, x25519_scalar_multiply
from simulator.memory import SparseAddressSpace


_ACCUMULATOR_QWORDS = X25519_BYTES // 8


@dataclass(slots=True)
class _FieldCoreState:
    accumulator: list[int] = field(
        default_factory=lambda: [0] * _ACCUMULATOR_QWORDS
    )
    operand_address: int = 0
    previous_low: int = 0


class HostedFieldALUService:
    """Runtime-local semantic Field ALU state keyed by physical core."""

    def __init__(self, *, core_count: int) -> None:
        if isinstance(core_count, bool) or not isinstance(core_count, int):
            raise TypeError("Field ALU core count must be an integer")
        if core_count <= 0:
            raise ValueError("Field ALU core count must be positive")
        self._states = tuple(_FieldCoreState() for _ in range(core_count))

    @property
    def core_count(self) -> int:
        return len(self._states)

    def accumulator(self, core_id: int) -> bytes:
        """Return one diagnostic ACC0-ACC3 snapshot in little-endian order."""

        state = self._state(core_id)
        return b"".join(value.to_bytes(8, "little") for value in state.accumulator)

    def operand_address(self, core_id: int) -> int:
        """Return one diagnostic TSRC0 snapshot."""

        return self._state(core_id).operand_address

    def previous_low(self, core_id: int) -> int:
        """Return the persistent low Field-ALU result used by later MAC words."""

        return self._state(core_id).previous_low

    def load_accumulator(
        self,
        core_id: int,
        address: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Load four qwords sequentially, matching the BIOS ``_gf_load_acc``."""

        state = self._state(core_id)
        memory = self._memory(memory)
        address = u64(address)
        for index in range(_ACCUMULATOR_QWORDS):
            state.accumulator[index] = memory.read64(
                u64(address + index * 8)
            )

    def set_operand_address(self, core_id: int, address: int) -> None:
        """Set TSRC0 without preflighting the deferred operand read."""

        self._state(core_id).operand_address = u64(address)

    def x25519(self, core_id: int, memory: SparseAddressSpace) -> None:
        """Replace ACC with RFC 7748 X25519(ACC, M[TSRC0])."""

        state = self._state(core_id)
        memory = self._memory(memory)
        point = b"".join(
            memory.read64(u64(state.operand_address + index * 8)).to_bytes(
                8,
                "little",
            )
            for index in range(_ACCUMULATOR_QWORDS)
        )
        scalar = b"".join(
            value.to_bytes(8, "little") for value in state.accumulator
        )
        result = x25519_scalar_multiply(scalar, point)
        result_value = int.from_bytes(result, "little")
        state.accumulator[:] = [
            (result_value >> (index * 64)) & MASK64
            for index in range(_ACCUMULATOR_QWORDS)
        ]
        state.previous_low = result_value

    def store_accumulator(
        self,
        core_id: int,
        address: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Store four qwords sequentially, matching the BIOS ``_gf_store_acc``."""

        state = self._state(core_id)
        memory = self._memory(memory)
        address = u64(address)
        for index, value in enumerate(state.accumulator):
            memory.write64(u64(address + index * 8), value)

    def reset(self) -> None:
        """Clear every admitted per-core Field-ALU register."""

        for state in self._states:
            state.accumulator[:] = [0] * _ACCUMULATOR_QWORDS
            state.operand_address = 0
            state.previous_low = 0

    def _state(self, core_id: int) -> _FieldCoreState:
        if isinstance(core_id, bool) or not isinstance(core_id, int):
            raise TypeError("Field ALU core ID must be an integer")
        if not 0 <= core_id < len(self._states):
            raise ValueError("Field ALU core ID is outside the hosted profile")
        return self._states[core_id]

    @staticmethod
    def _memory(memory: SparseAddressSpace) -> SparseAddressSpace:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("Field ALU memory must be a SparseAddressSpace")
        return memory


__all__ = ["HostedFieldALUService"]
