"""Portable tile values and the hosted legacy tile-engine service.

The value helpers are shared by hosted diagnostics.  :class:`HostedTileService`
adds the retained pseudo-BIOS register state needed by ordinary MegaForth
source, but deliberately does not model instruction encoding, latency,
scratchpad arbitration, or a physical datapath.
"""

from __future__ import annotations

import math
from collections.abc import Callable, Iterable
from typing import Protocol

from shared.cells import MASK64, u64
from shared.fp import (
    BF16_FORMAT,
    FP16_FORMAT,
    bits_to_fp32,
    decode_tile_float,
    encode_tile_float,
    fp32_to_bits,
    tile_float_is_nan,
)
from simulator.errors import ExecutionError
from simulator.memory import SparseAddressSpace


TILE_BYTES = 64
ACCUMULATOR_WORDS = 4
_ACCUMULATOR_MASK = (1 << (ACCUMULATOR_WORDS * 64)) - 1


class _LegacyRegisterFile(Protocol):
    """The ACC/TSRC0/TDST subset shared with the hosted Field ALU."""

    def accumulator_words(self, core_id: int) -> tuple[int, ...]: ...

    def replace_accumulator_words(
        self,
        core_id: int,
        words: Iterable[int],
    ) -> None: ...

    def operand_address(self, core_id: int) -> int: ...

    def result_address(self, core_id: int) -> int: ...

    def set_operand_address(self, core_id: int, address: int) -> None: ...

    def set_result_address(self, core_id: int, address: int) -> None: ...


class UnsupportedTileModeError(ExecutionError):
    """An operation reached a tile format outside the admitted format set."""

    def __init__(self, mode: int) -> None:
        self.mode = mode
        super().__init__(
            f"tile mode 0x{mode:02x} is not admitted by the hosted tile service"
        )


def _tile_bytes(value: bytes, *, label: str) -> bytes:
    if not isinstance(value, bytes):
        raise TypeError(f"{label} must be bytes")
    if len(value) != TILE_BYTES:
        raise ValueError(f"{label} must contain exactly {TILE_BYTES} lanes")
    return value


def tile_add_u8(left: bytes, right: bytes) -> bytes:
    """Return wrapping unsigned 8-bit lane addition."""

    left = _tile_bytes(left, label="left tile")
    right = _tile_bytes(right, label="right tile")
    return bytes((a + b) & 0xFF for a, b in zip(left, right))


def tile_multiply_u8(left: bytes, right: bytes) -> bytes:
    """Return wrapping unsigned 8-bit lane multiplication."""

    left = _tile_bytes(left, label="left tile")
    right = _tile_bytes(right, label="right tile")
    return bytes((a * b) & 0xFF for a, b in zip(left, right))


def tile_dot_u8(left: bytes, right: bytes) -> int:
    """Return the wrapped cell sum of unsigned lane products."""

    left = _tile_bytes(left, label="left tile")
    right = _tile_bytes(right, label="right tile")
    return u64(sum(a * b for a, b in zip(left, right)))


def tile_sum_u8(tile: bytes) -> int:
    """Return the wrapped cell sum of unsigned lanes."""

    tile = _tile_bytes(tile, label="tile")
    return u64(sum(tile))


class HostedTileService:
    """One runtime-local semantic legacy tile engine.

    The service accepts all four integer element widths, FP16, and BF16.  It
    implements the legacy and extended BIOS operations reached by ordinary
    source.  The separately owned full-width TACC family remains unsupported.
    """

    __slots__ = (
        "_account_operation",
        "_control",
        "_core_id",
        "_memory",
        "_mode",
        "_registers",
        "_source1",
    )

    def __init__(
        self,
        memory: SparseAddressSpace,
        registers: _LegacyRegisterFile,
        *,
        core_id: int = 0,
        account_operation: Callable[[], None] | None = None,
    ) -> None:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("tile memory must be a SparseAddressSpace")
        if isinstance(core_id, bool) or not isinstance(core_id, int):
            raise TypeError("tile core ID must be an integer")
        if core_id < 0:
            raise ValueError("tile core ID must not be negative")
        if account_operation is not None and not callable(account_operation):
            raise TypeError("tile operation accountant must be callable or None")
        self._memory = memory
        self._registers = registers
        self._core_id = core_id
        self._account_operation = account_operation
        self._mode = 0
        self._control = 0
        self._source1 = 0
        # Validate the injected shared-register view at construction.
        self._registers.accumulator_words(core_id)

    @property
    def mode(self) -> int:
        return self._mode

    @property
    def control(self) -> int:
        return self._control

    @property
    def source0(self) -> int:
        return self._registers.operand_address(self._core_id)

    @property
    def source1(self) -> int:
        return self._source1

    @property
    def destination(self) -> int:
        return self._registers.result_address(self._core_id)

    @property
    def accumulator(self) -> tuple[int, ...]:
        """Return an immutable low-to-high ACC0--ACC3 snapshot."""

        return self._registers.accumulator_words(self._core_id)

    def set_mode(self, value: int) -> None:
        self._mode = self._cell(value, label="tile mode") & 0xFF

    def set_control(self, value: int) -> None:
        self._control = self._cell(value, label="tile control") & 0xFF

    def set_source0(self, address: int) -> None:
        self._registers.set_operand_address(self._core_id, address)

    def set_source1(self, address: int) -> None:
        self._source1 = self._cell(address, label="tile source 1")

    def set_destination(self, address: int) -> None:
        self._registers.set_result_address(self._core_id, address)

    def accumulator_word(self, index: int = 0) -> int:
        if isinstance(index, bool) or not isinstance(index, int):
            raise TypeError("tile accumulator index must be an integer")
        if not 0 <= index < ACCUMULATOR_WORDS:
            raise ValueError("tile accumulator index must be from 0 through 3")
        return self.accumulator[index]

    def add(self) -> None:
        self._binary("add")

    def subtract(self) -> None:
        self._binary("subtract")

    def bitwise_and(self) -> None:
        self._binary("bitwise_and")

    def bitwise_or(self) -> None:
        self._binary("bitwise_or")

    def bitwise_xor(self) -> None:
        self._binary("bitwise_xor")

    def elementwise_minimum(self) -> None:
        self._binary("minimum")

    def elementwise_maximum(self) -> None:
        self._binary("maximum")

    def absolute(self) -> None:
        self._binary("absolute")

    def multiply(self) -> None:
        self._binary("multiply")

    def widening_multiply(self) -> None:
        element_bytes, signed, _saturating, floating_format = self._mode_format()
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)
        output0 = bytearray(TILE_BYTES)
        output1 = bytearray(TILE_BYTES)

        if floating_format is not None:
            output_bytes = 4
            for lane, offset in enumerate(range(0, TILE_BYTES, element_bytes)):
                raw_left = int.from_bytes(
                    left[offset : offset + element_bytes],
                    "little",
                )
                raw_right = int.from_bytes(
                    right[offset : offset + element_bytes],
                    "little",
                )
                result = fp32_to_bits(
                    float(decode_tile_float(raw_left, floating_format))
                    * float(decode_tile_float(raw_right, floating_format))
                )
                self._set_wide_lane(
                    output0,
                    output1,
                    lane,
                    output_bytes,
                    result,
                )
        else:
            bits = element_bytes * 8
            output_bytes = element_bytes * 2
            output_mask = (1 << (output_bytes * 8)) - 1
            for lane, offset in enumerate(range(0, TILE_BYTES, element_bytes)):
                raw_left = int.from_bytes(
                    left[offset : offset + element_bytes],
                    "little",
                )
                raw_right = int.from_bytes(
                    right[offset : offset + element_bytes],
                    "little",
                )
                lane_left = self._signed(raw_left, bits) if signed else raw_left
                lane_right = self._signed(raw_right, bits) if signed else raw_right
                self._set_wide_lane(
                    output0,
                    output1,
                    lane,
                    output_bytes,
                    lane_left * lane_right & output_mask,
                )

        self._write_wide_result(output0, output1)
        self._account()

    def multiply_accumulate(self) -> None:
        self._multiply_add()

    def fused_multiply_add(self) -> None:
        self._multiply_add()

    def dot(self) -> None:
        element_bytes, signed, _saturating, floating_format = self._mode_format()
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)

        if floating_format is not None:
            total = 0.0
            for offset in range(0, TILE_BYTES, element_bytes):
                raw_left = int.from_bytes(
                    left[offset : offset + element_bytes],
                    "little",
                )
                raw_right = int.from_bytes(
                    right[offset : offset + element_bytes],
                    "little",
                )
                total += float(decode_tile_float(raw_left, floating_format)) * float(
                    decode_tile_float(raw_right, floating_format)
                )
            self._publish_float_reduction(total, accumulate=True)
        else:
            bits = element_bytes * 8
            total = 0
            for offset in range(0, TILE_BYTES, element_bytes):
                raw_left = int.from_bytes(
                    left[offset : offset + element_bytes],
                    "little",
                )
                raw_right = int.from_bytes(
                    right[offset : offset + element_bytes],
                    "little",
                )
                lane_left = self._signed(raw_left, bits) if signed else raw_left
                lane_right = self._signed(raw_right, bits) if signed else raw_right
                total += lane_left * lane_right
            self._publish_integer_reduction(total)
        self._account()

    def sum(self) -> None:
        self._reduce("sum")

    def minimum(self) -> None:
        self._reduce("minimum")

    def maximum(self) -> None:
        self._reduce("maximum")

    def sum_squares(self) -> None:
        self._reduce("sum_squares")

    def popcount(self) -> None:
        element_bytes, _signed, _saturating, _floating_format = (
            self._mode_format()
        )
        tile = self._memory.read_bytes(self.source0, TILE_BYTES)
        result = sum(
            int.from_bytes(
                tile[offset : offset + element_bytes],
                "little",
            ).bit_count()
            for offset in range(0, TILE_BYTES, element_bytes)
        )
        self._publish_integer_reduction(result)
        self._account()

    def l1_norm(self) -> None:
        element_bytes, signed, _saturating, floating_format = self._mode_format()
        if floating_format is not None:
            # The executable machine retains its integer fallback for this
            # otherwise unsupported FP reduction.  TMODE.SIGNED therefore
            # selects two's-complement interpretation of raw 16-bit lanes.
            signed = bool(self._mode & 0x10)
        bits = element_bytes * 8
        tile = self._memory.read_bytes(self.source0, TILE_BYTES)
        raw_values = (
            int.from_bytes(
                tile[offset : offset + element_bytes],
                "little",
            )
            for offset in range(0, TILE_BYTES, element_bytes)
        )
        if signed:
            result = sum(abs(self._signed(value, bits)) for value in raw_values)
        else:
            result = sum(raw_values)
        self._publish_integer_reduction(result)
        self._account()

    def minimum_index(self) -> None:
        self._index_reduce(minimum=True)

    def maximum_index(self) -> None:
        self._index_reduce(minimum=False)

    def transpose(self) -> None:
        tile = self._memory.read_bytes(self.destination, TILE_BYTES)
        output = bytearray(TILE_BYTES)
        for row in range(8):
            for column in range(8):
                output[column * 8 + row] = tile[row * 8 + column]
        self._memory.write_bytes(self.destination, output)
        self._account()

    def _binary(self, operation: str) -> None:
        element_bytes, signed, saturating, floating_format = self._mode_format()
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)
        bits = element_bytes * 8
        lane_mask = (1 << bits) - 1
        low = -(1 << (bits - 1))
        high = (1 << (bits - 1)) - 1
        output = bytearray(TILE_BYTES)

        for offset in range(0, TILE_BYTES, element_bytes):
            raw_left = int.from_bytes(left[offset : offset + element_bytes], "little")
            raw_right = int.from_bytes(
                right[offset : offset + element_bytes],
                "little",
            )
            if floating_format is not None:
                if operation == "bitwise_and":
                    encoded = raw_left & raw_right
                elif operation == "bitwise_or":
                    encoded = raw_left | raw_right
                elif operation == "bitwise_xor":
                    encoded = raw_left ^ raw_right
                elif operation == "absolute":
                    encoded = raw_left & 0x7FFF
                elif operation in ("minimum", "maximum"):
                    if tile_float_is_nan(
                        raw_left,
                        floating_format,
                    ) or tile_float_is_nan(raw_right, floating_format):
                        encoded = (
                            0x7E00
                            if floating_format == FP16_FORMAT
                            else 0x7FC0
                        )
                    else:
                        lane_left_float = decode_tile_float(
                            raw_left,
                            floating_format,
                        )
                        lane_right_float = decode_tile_float(
                            raw_right,
                            floating_format,
                        )
                        encoded = encode_tile_float(
                            min(lane_left_float, lane_right_float)
                            if operation == "minimum"
                            else max(lane_left_float, lane_right_float),
                            floating_format,
                        )
                else:
                    lane_left_float = decode_tile_float(
                        raw_left,
                        floating_format,
                    )
                    lane_right_float = decode_tile_float(
                        raw_right,
                        floating_format,
                    )
                    if operation == "add":
                        result = lane_left_float + lane_right_float
                    elif operation == "subtract":
                        result = lane_left_float - lane_right_float
                    elif operation == "multiply":
                        result = lane_left_float * lane_right_float
                    else:  # pragma: no cover - private callers constrain this value
                        raise AssertionError(
                            f"unknown tile binary operation {operation!r}"
                        )
                    encoded = encode_tile_float(result, floating_format)
                output[offset : offset + element_bytes] = encoded.to_bytes(
                    element_bytes,
                    "little",
                )
                continue

            if operation in ("minimum", "maximum", "absolute") or (
                saturating and signed
            ):
                lane_left = self._signed(raw_left, bits)
                lane_right = self._signed(raw_right, bits)
            else:
                lane_left = raw_left
                lane_right = raw_right
            if operation == "add":
                result = lane_left + lane_right
                if saturating:
                    result = (
                        max(low, min(high, result))
                        if signed
                        else min(lane_mask, result)
                    )
            elif operation == "subtract":
                result = lane_left - lane_right
                if saturating:
                    result = max(low, min(high, result)) if signed else max(0, result)
            elif operation == "multiply":
                lane_left = self._signed(raw_left, bits) if signed else raw_left
                lane_right = self._signed(raw_right, bits) if signed else raw_right
                result = lane_left * lane_right
            elif operation == "bitwise_and":
                result = raw_left & raw_right
            elif operation == "bitwise_or":
                result = raw_left | raw_right
            elif operation == "bitwise_xor":
                result = raw_left ^ raw_right
            elif operation == "minimum":
                result = (
                    min(lane_left, lane_right)
                    if signed
                    else min(raw_left, raw_right)
                )
            elif operation == "maximum":
                result = (
                    max(lane_left, lane_right)
                    if signed
                    else max(raw_left, raw_right)
                )
            elif operation == "absolute":
                result = abs(lane_left) if signed else raw_left
            else:  # pragma: no cover - private callers constrain this value
                raise AssertionError(f"unknown tile binary operation {operation!r}")
            output[offset : offset + element_bytes] = (result & lane_mask).to_bytes(
                element_bytes,
                "little",
            )

        self._memory.write_bytes(self.destination, output)
        self._account()

    def _multiply_add(self) -> None:
        element_bytes, signed, _saturating, floating_format = self._mode_format()
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)
        existing = self._memory.read_bytes(self.destination, TILE_BYTES)
        bits = element_bytes * 8
        lane_mask = (1 << bits) - 1
        output = bytearray(TILE_BYTES)

        for offset in range(0, TILE_BYTES, element_bytes):
            raw_left = int.from_bytes(left[offset : offset + element_bytes], "little")
            raw_right = int.from_bytes(
                right[offset : offset + element_bytes],
                "little",
            )
            raw_existing = int.from_bytes(
                existing[offset : offset + element_bytes],
                "little",
            )
            if floating_format is not None:
                result = (
                    float(decode_tile_float(raw_left, floating_format))
                    * float(decode_tile_float(raw_right, floating_format))
                    + float(decode_tile_float(raw_existing, floating_format))
                )
                encoded = encode_tile_float(result, floating_format)
            else:
                lane_left = self._signed(raw_left, bits) if signed else raw_left
                lane_right = self._signed(raw_right, bits) if signed else raw_right
                lane_existing = (
                    self._signed(raw_existing, bits) if signed else raw_existing
                )
                encoded = (lane_left * lane_right + lane_existing) & lane_mask
            output[offset : offset + element_bytes] = encoded.to_bytes(
                element_bytes,
                "little",
            )

        self._memory.write_bytes(self.destination, output)
        self._account()

    def _reduce(self, operation: str) -> None:
        element_bytes, signed, _saturating, floating_format = self._mode_format()
        tile = self._memory.read_bytes(self.source0, TILE_BYTES)
        raw_values = [
            int.from_bytes(tile[offset : offset + element_bytes], "little")
            for offset in range(0, TILE_BYTES, element_bytes)
        ]

        if floating_format is not None:
            values = [
                decode_tile_float(value, floating_format) for value in raw_values
            ]
            if operation == "sum":
                result = sum(float(value) for value in values)
                accumulate = True
            elif operation == "minimum":
                non_nan = [value for value in values if not math.isnan(value)]
                result = min(non_nan) if non_nan else float("nan")
                accumulate = False
            elif operation == "maximum":
                non_nan = [value for value in values if not math.isnan(value)]
                result = max(non_nan) if non_nan else float("nan")
                accumulate = False
            elif operation == "sum_squares":
                result = sum(float(value) * float(value) for value in values)
                accumulate = True
            else:  # pragma: no cover - private callers constrain this value
                raise AssertionError(f"unknown tile reduction {operation!r}")
            self._publish_float_reduction(result, accumulate=accumulate)
            self._account()
            return

        bits = element_bytes * 8
        values = raw_values
        if signed:
            values = [self._signed(value, bits) for value in values]

        if operation == "sum":
            result = sum(values)
        elif operation == "minimum":
            result = min(values)
        elif operation == "maximum":
            result = max(values)
        elif operation == "sum_squares":
            result = sum(value * value for value in values)
        else:  # pragma: no cover - private callers constrain this value
            raise AssertionError(f"unknown tile reduction {operation!r}")

        self._publish_integer_reduction(result)
        self._account()

    def _index_reduce(self, *, minimum: bool) -> None:
        element_bytes, signed, _saturating, floating_format = self._mode_format()
        tile = self._memory.read_bytes(self.source0, TILE_BYTES)
        raw_values = [
            int.from_bytes(tile[offset : offset + element_bytes], "little")
            for offset in range(0, TILE_BYTES, element_bytes)
        ]

        if floating_format is not None:
            values = [
                decode_tile_float(value, floating_format) for value in raw_values
            ]
            best_index = 0
            best_value = values[0]
            for index, value in enumerate(values[1:], start=1):
                if math.isnan(value):
                    continue
                if math.isnan(best_value) or (
                    value < best_value if minimum else value > best_value
                ):
                    best_index = index
                    best_value = value
            self._registers.replace_accumulator_words(
                self._core_id,
                (best_index, fp32_to_bits(best_value), 0, 0),
            )
            if self._control & 0x02:
                self._control &= ~0x02
            self._account()
            return

        bits = element_bytes * 8
        values = raw_values
        if signed:
            values = [self._signed(value, bits) for value in raw_values]
        best_index = 0
        best_value = values[0]
        for index, value in enumerate(values[1:], start=1):
            if value < best_value if minimum else value > best_value:
                best_index = index
                best_value = value

        control = self._control
        words = list(self.accumulator)
        if control & 0x02:
            words = [0] * ACCUMULATOR_WORDS
            self._control &= ~0x02
        replace = not control & 0x01
        if not replace:
            old_value = self._signed(words[1], 64) if signed else words[1]
            replace = (
                best_value < old_value if minimum else best_value > old_value
            )
        if replace:
            words[0] = best_index
            words[1] = best_value & MASK64
        self._registers.replace_accumulator_words(self._core_id, words)
        self._account()

    @staticmethod
    def _set_wide_lane(
        output0: bytearray,
        output1: bytearray,
        lane: int,
        element_bytes: int,
        value: int,
    ) -> None:
        lanes_per_tile = TILE_BYTES // element_bytes
        output = output0 if lane < lanes_per_tile else output1
        output_lane = lane if lane < lanes_per_tile else lane - lanes_per_tile
        offset = output_lane * element_bytes
        output[offset : offset + element_bytes] = value.to_bytes(
            element_bytes,
            "little",
        )

    def _write_wide_result(
        self,
        output0: bytearray,
        output1: bytearray,
    ) -> None:
        destination0 = self.destination
        destination1 = u64(destination0 + TILE_BYTES)
        # Architectural WMUL publishes two ordered 64-byte writes.  A fault on
        # the second span therefore leaves the first tile visible.
        self._memory.write_bytes(destination0, output0)
        self._memory.write_bytes(destination1, output1)

    def _publish_integer_reduction(self, result: int) -> None:
        control = self._control
        if control & 0x01:
            old = 0 if control & 0x02 else self._accumulator_value()
            result += old
        result &= _ACCUMULATOR_MASK
        words = tuple(
            (result >> (index * 64)) & MASK64
            for index in range(ACCUMULATOR_WORDS)
        )
        self._registers.replace_accumulator_words(self._core_id, words)
        if control & 0x02:
            self._control &= ~0x02

    def _publish_float_reduction(
        self,
        result: float,
        *,
        accumulate: bool,
    ) -> None:
        control = self._control
        if accumulate and control & 0x01:
            old_bits = 0 if control & 0x02 else self.accumulator[0]
            result = bits_to_fp32(old_bits) + result
        self._registers.replace_accumulator_words(
            self._core_id,
            (fp32_to_bits(result), 0, 0, 0),
        )
        if control & 0x02:
            self._control &= ~0x02

    def _mode_format(self) -> tuple[int, bool, bool, int | None]:
        element_width = self._mode & 0x07
        if element_width <= 3:
            return (
                1 << element_width,
                bool(self._mode & 0x10),
                bool(self._mode & 0x20),
                None,
            )
        if element_width in (FP16_FORMAT, BF16_FORMAT):
            return 2, False, False, element_width
        raise UnsupportedTileModeError(self._mode)

    def _accumulator_value(self) -> int:
        return sum(
            word << (index * 64)
            for index, word in enumerate(self.accumulator)
        )

    def _account(self) -> None:
        if self._account_operation is not None:
            self._account_operation()

    @staticmethod
    def _signed(value: int, bits: int) -> int:
        sign = 1 << (bits - 1)
        return value - (1 << bits) if value & sign else value

    @staticmethod
    def _cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer")
        return u64(value)


__all__ = [
    "ACCUMULATOR_WORDS",
    "HostedTileService",
    "TILE_BYTES",
    "UnsupportedTileModeError",
    "tile_add_u8",
    "tile_dot_u8",
    "tile_multiply_u8",
    "tile_sum_u8",
]
