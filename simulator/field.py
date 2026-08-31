"""Hosted per-core Field-ALU state needed by semantic BIOS words.

The service preserves ACC0-ACC3, TSRC0, TDST, prime configuration, and
previous-result state across ordinary BIOS calls.  It models terminal values
and sequential guest-memory effects without emulating EXT.CRYPTO encodings,
cycles, stalls, or the RTL datapath.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable

from shared.cells import MASK64, u64
from shared.field import (
    FIELD_BYTES,
    FIELD_MASK,
    active_prime,
    field_add,
    field_inverse,
    field_multiply,
    field_power,
    field_square,
    field_subtract,
    montgomery_multiply,
    raw_multiply_add,
    raw_product,
)
from shared.x25519 import x25519_scalar_multiply
from simulator.memory import SparseAddressSpace


_ACCUMULATOR_QWORDS = FIELD_BYTES // 8
_BinaryOperation = Callable[[int, int, int], int]


@dataclass(slots=True)
class _FieldCoreState:
    accumulator: list[int] = field(
        default_factory=lambda: [0] * _ACCUMULATOR_QWORDS
    )
    operand_address: int = 0
    result_address: int = 0
    prime_selection: int = 0
    custom_prime: int = 0
    montgomery_inverse: int = 0
    previous_low: int = 0
    previous_high: int = 0
    zero_flag: bool = False


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

    def result_address(self, core_id: int) -> int:
        """Return one diagnostic TDST snapshot."""

        return self._state(core_id).result_address

    def prime_selection(self, core_id: int) -> int:
        """Return the selected built-in/custom-prime index."""

        return self._state(core_id).prime_selection

    def custom_prime(self, core_id: int) -> int:
        """Return the latched custom prime."""

        return self._state(core_id).custom_prime

    def montgomery_inverse(self, core_id: int) -> int:
        """Return the latched custom Montgomery negative inverse."""

        return self._state(core_id).montgomery_inverse

    def previous_low(self, core_id: int) -> int:
        """Return the persistent low Field-ALU result."""

        return self._state(core_id).previous_low

    def previous_high(self, core_id: int) -> int:
        """Return the persistent high raw-result half."""

        return self._state(core_id).previous_high

    def zero_flag(self, core_id: int) -> bool:
        """Return raw GF.CEQ Z for diagnostics, before BIOS flag clobbering."""

        return self._state(core_id).zero_flag

    def load_accumulator(
        self,
        core_id: int,
        address: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Load four qwords sequentially, matching BIOS ``_gf_load_acc``."""

        state = self._state(core_id)
        memory = self._memory(memory)
        address = self._cell(address, label="Field accumulator source")
        for index in range(_ACCUMULATOR_QWORDS):
            state.accumulator[index] = memory.read64(
                u64(address + index * 8)
            )

    def store_accumulator(
        self,
        core_id: int,
        address: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Store four qwords sequentially, matching BIOS ``_gf_store_acc``."""

        state = self._state(core_id)
        memory = self._memory(memory)
        address = self._cell(address, label="Field accumulator destination")
        for index, value in enumerate(state.accumulator):
            memory.write64(u64(address + index * 8), value)

    def set_operand_address(self, core_id: int, address: int) -> None:
        """Set TSRC0 without preflighting the deferred operand read."""

        self._state(core_id).operand_address = self._cell(
            address,
            label="Field operand address",
        )

    def set_result_address(self, core_id: int, address: int) -> None:
        """Set TDST without preflighting the deferred high-result write."""

        self._state(core_id).result_address = self._cell(
            address,
            label="Field result address",
        )

    def select_prime(self, core_id: int, selection: int) -> None:
        """Select one of four prime modes through the low two cell bits."""

        selection = self._cell(selection, label="Field prime selection")
        self._state(core_id).prime_selection = selection & 0x03

    def latch_custom_prime(
        self,
        core_id: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Latch custom p from ACC, then p_inv from deferred TSRC0."""

        state = self._state(core_id)
        memory = self._memory(memory)
        state.custom_prime = self._accumulator_value(state)
        state.montgomery_inverse = self._read_operand(state, memory)

    def add(self, core_id: int, memory: SparseAddressSpace) -> None:
        """Apply the selected prime's one-subtraction add and publish low."""

        self._binary_result(core_id, memory, field_add)

    def subtract(self, core_id: int, memory: SparseAddressSpace) -> None:
        """Apply the selected prime's subtract-or-compensate operation."""

        self._binary_result(core_id, memory, field_subtract)

    def multiply(self, core_id: int, memory: SparseAddressSpace) -> None:
        """Replace ACC with the selected ordinary/Montgomery product."""

        state = self._state(core_id)
        operand = self._read_operand(state, self._memory(memory))
        self._publish_low(
            state,
            self._selected_product(
                state,
                self._accumulator_value(state),
                operand,
            ),
        )

    def square(self, core_id: int) -> None:
        """Replace ACC with the selected ordinary/Montgomery square."""

        state = self._state(core_id)
        value = self._accumulator_value(state)
        prime = self._prime(state)
        if self._uses_montgomery(state):
            result = montgomery_multiply(
                value,
                value,
                prime,
                state.montgomery_inverse,
            )
        else:
            result = field_square(value, prime)
        self._publish_low(state, result)

    def invert(self, core_id: int) -> None:
        """Replace ACC with its selected-prime Fermat exponent result."""

        state = self._state(core_id)
        result = field_inverse(
            self._accumulator_value(state),
            self._prime(state),
        )
        self._publish_low(state, result)

    def power(self, core_id: int, memory: SparseAddressSpace) -> None:
        """Replace ACC with ``ACC**B mod p``."""

        state = self._state(core_id)
        exponent = self._read_operand(state, self._memory(memory))
        result = field_power(
            self._accumulator_value(state),
            exponent,
            self._prime(state),
        )
        self._publish_low(state, result)

    def multiply_raw(
        self,
        core_id: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Publish high through TDST, retain low in ACC, then update previous."""

        state = self._state(core_id)
        memory = self._memory(memory)
        operand = self._read_operand(state, memory)
        low, high = raw_product(self._accumulator_value(state), operand)
        self._replace_accumulator(state, low)
        self._store_high(state, high, memory)
        state.previous_low = low
        state.previous_high = high

    def conditional_move(
        self,
        core_id: int,
        condition: bool,
        memory: SparseAddressSpace,
    ) -> None:
        """Read B unconditionally and replace ACC/previous-low when true."""

        if not isinstance(condition, bool):
            raise TypeError("Field conditional-move condition must be boolean")
        state = self._state(core_id)
        operand = self._read_operand(state, self._memory(memory))
        if condition:
            self._publish_low(state, operand)

    def equal(self, core_id: int, memory: SparseAddressSpace) -> None:
        """Replace ACC with 1/0, update previous-low, and record Z."""

        state = self._state(core_id)
        operand = self._read_operand(state, self._memory(memory))
        result = int(self._accumulator_value(state) == operand)
        self._publish_low(state, result)
        state.zero_flag = bool(result)

    def multiply_accumulate(
        self,
        core_id: int,
        memory: SparseAddressSpace,
    ) -> None:
        """One-subtraction-add selected product to retained previous-low."""

        state = self._state(core_id)
        operand = self._read_operand(state, self._memory(memory))
        prime = self._prime(state)
        product = self._selected_product(
            state,
            self._accumulator_value(state),
            operand,
        )
        self._publish_low(state, field_add(product, state.previous_low, prime))

    def multiply_add_raw(
        self,
        core_id: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Add ACC*B to previous 512-bit state with wrapped publication."""

        state = self._state(core_id)
        memory = self._memory(memory)
        operand = self._read_operand(state, memory)
        low, high = raw_multiply_add(
            self._accumulator_value(state),
            operand,
            state.previous_low,
            state.previous_high,
        )
        self._replace_accumulator(state, low)
        self._store_high(state, high, memory)
        state.previous_low = low
        state.previous_high = high

    def x25519(self, core_id: int, memory: SparseAddressSpace) -> None:
        """Replace ACC with RFC 7748 X25519(ACC, M[TSRC0])."""

        state = self._state(core_id)
        point_value = self._read_operand(state, self._memory(memory))
        scalar = self.accumulator(core_id)
        point = point_value.to_bytes(FIELD_BYTES, "little")
        result = x25519_scalar_multiply(scalar, point)
        result_value = int.from_bytes(result, "little")
        self._replace_accumulator(state, result_value)
        state.previous_low = result_value

    def reset(self) -> None:
        """Clear every admitted per-core Field-ALU register."""

        for state in self._states:
            state.accumulator[:] = [0] * _ACCUMULATOR_QWORDS
            state.operand_address = 0
            state.result_address = 0
            state.prime_selection = 0
            state.custom_prime = 0
            state.montgomery_inverse = 0
            state.previous_low = 0
            state.previous_high = 0
            state.zero_flag = False

    def _binary_result(
        self,
        core_id: int,
        memory: SparseAddressSpace,
        operation: _BinaryOperation,
    ) -> None:
        state = self._state(core_id)
        operand = self._read_operand(state, self._memory(memory))
        result = operation(
            self._accumulator_value(state),
            operand,
            self._prime(state),
        )
        self._publish_low(state, result)

    @staticmethod
    def _accumulator_value(state: _FieldCoreState) -> int:
        return sum(
            value << (index * 64)
            for index, value in enumerate(state.accumulator)
        )

    @staticmethod
    def _replace_accumulator(state: _FieldCoreState, value: int) -> None:
        value &= FIELD_MASK
        state.accumulator[:] = [
            (value >> (index * 64)) & MASK64
            for index in range(_ACCUMULATOR_QWORDS)
        ]

    def _publish_low(self, state: _FieldCoreState, value: int) -> None:
        self._replace_accumulator(state, value)
        state.previous_low = value & FIELD_MASK

    @staticmethod
    def _read_operand(
        state: _FieldCoreState,
        memory: SparseAddressSpace,
    ) -> int:
        return sum(
            memory.read64(u64(state.operand_address + index * 8))
            << (index * 64)
            for index in range(_ACCUMULATOR_QWORDS)
        )

    @staticmethod
    def _store_high(
        state: _FieldCoreState,
        value: int,
        memory: SparseAddressSpace,
    ) -> None:
        for index in range(_ACCUMULATOR_QWORDS):
            memory.write64(
                u64(state.result_address + index * 8),
                value >> (index * 64),
            )

    @staticmethod
    def _prime(state: _FieldCoreState) -> int:
        return active_prime(state.prime_selection, state.custom_prime)

    @staticmethod
    def _uses_montgomery(state: _FieldCoreState) -> bool:
        return state.prime_selection == 3 and state.montgomery_inverse != 0

    def _selected_product(
        self,
        state: _FieldCoreState,
        first: int,
        second: int,
    ) -> int:
        prime = self._prime(state)
        if self._uses_montgomery(state):
            return montgomery_multiply(
                first,
                second,
                prime,
                state.montgomery_inverse,
            )
        return field_multiply(first, second, prime)

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

    @staticmethod
    def _cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer")
        return u64(value)


__all__ = ["HostedFieldALUService"]
