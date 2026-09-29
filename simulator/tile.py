"""Portable tile values and the hosted legacy tile-engine service.

The value helpers are shared by hosted diagnostics.  :class:`HostedTileService`
adds the retained pseudo-BIOS register state needed by ordinary MegaForth
source, but deliberately does not model instruction encoding, latency,
scratchpad arbitration, or a physical datapath.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable
from typing import Protocol

from shared import ieee_fp, tile_float, tile_formats
from shared.cells import MASK64, u64
from simulator.errors import ExecutionError
from simulator.memory import SparseAddressSpace


TILE_BYTES = 64
ACCUMULATOR_WORDS = 4
_ACCUMULATOR_MASK = (1 << (ACCUMULATOR_WORDS * 64)) - 1
_REDUCTION_FUNCTIONS = {
    "sum": tile_formats.TRED_SUM,
    "minimum": tile_formats.TRED_MIN,
    "maximum": tile_formats.TRED_MAX,
    "sum_squares": tile_formats.TRED_SUMSQ,
}
_TALU_FUNCTIONS = {
    "add": tile_float.ADD,
    "subtract": tile_float.SUB,
    "bitwise_and": tile_float.AND,
    "bitwise_or": tile_float.OR,
    "bitwise_xor": tile_float.XOR,
    "minimum": tile_float.MIN,
    "maximum": tile_float.MAX,
    "absolute": tile_float.ABS,
}


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
    """A tile operation reached a format that does not admit it."""

    def __init__(self, mode: int) -> None:
        self.mode = mode
        super().__init__(
            f"tile mode 0x{mode:02x} does not admit this hosted tile operation"
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

    The service accepts every defined format and fails closed on the reserved
    codes and on operations a format does not admit (``tile_formats.admits``).
    It
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
        self._mode = (
            self._cell(value, label="tile mode") & tile_formats.TMODE_WRITE_MASK
        )

    def set_control(self, value: int) -> None:
        self._control = (
            self._cell(value, label="tile control")
            & tile_formats.TCTRL_WRITE_MASK
        )

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
        element_bytes, signed, _saturating, floating_format = self._mode_format(
            tile_formats.TMUL, tile_formats.TMUL_WMUL
        )
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)
        output0 = bytearray(TILE_BYTES)
        output1 = bytearray(TILE_BYTES)

        if floating_format is not None:
            products = tile_float.pack_lanes(
                ieee_fp.accumulation_format(floating_format),
                tile_float.widening_multiply(
                    floating_format,
                    tile_float.unpack_lanes(floating_format, left),
                    tile_float.unpack_lanes(floating_format, right),
                ),
            )
            output0[:] = products[:TILE_BYTES]
            output1[:] = products[TILE_BYTES:]
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
        element_bytes, signed, _saturating, floating_format = self._mode_format(
            tile_formats.TMUL, tile_formats.TMUL_DOT
        )
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)

        if floating_format is not None:
            self._publish_float_sum(
                floating_format,
                tile_float.dot(
                    floating_format,
                    tile_float.unpack_lanes(floating_format, left),
                    tile_float.unpack_lanes(floating_format, right),
                ),
            )
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
            self._mode_format(tile_formats.TRED, tile_formats.TRED_POPCNT)
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
        element_bytes, signed, _saturating, floating_format = self._mode_format(
            tile_formats.TRED, tile_formats.TRED_L1
        )
        tile = self._memory.read_bytes(self.source0, TILE_BYTES)
        if floating_format is not None:
            self._publish_float_sum(
                floating_format,
                tile_float.l1_norm(
                    floating_format,
                    tile_float.unpack_lanes(floating_format, tile),
                ),
            )
            self._account()
            return
        bits = element_bytes * 8
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
        self._mode_format(tile_formats.TSYS, tile_formats.TSYS_TRANS)
        tile = self._memory.read_bytes(self.destination, TILE_BYTES)
        output = bytearray(TILE_BYTES)
        for row in range(8):
            for column in range(8):
                output[column * 8 + row] = tile[row * 8 + column]
        self._memory.write_bytes(self.destination, output)
        self._account()

    def _binary(self, operation: str) -> None:
        if operation == "multiply":
            op, funct = tile_formats.TMUL, tile_formats.TMUL_MUL
        else:
            op, funct = tile_formats.TALU, _TALU_FUNCTIONS[operation]
        element_bytes, signed, saturating, floating_format = self._mode_format(
            op, funct
        )
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)
        bits = element_bytes * 8
        lane_mask = (1 << bits) - 1
        low = -(1 << (bits - 1))
        high = (1 << (bits - 1)) - 1
        output = bytearray(TILE_BYTES)

        if floating_format is not None:
            left_lanes = tile_float.unpack_lanes(floating_format, left)
            right_lanes = tile_float.unpack_lanes(floating_format, right)
            if operation == "multiply":
                lanes = tile_float.multiply(
                    floating_format, left_lanes, right_lanes
                )
            else:
                lanes = tile_float.elementwise(
                    floating_format,
                    _TALU_FUNCTIONS[operation],
                    left_lanes,
                    right_lanes,
                )
            self._memory.write_bytes(
                self.destination,
                tile_float.pack_lanes(floating_format, lanes),
            )
            self._account()
            return

        for offset in range(0, TILE_BYTES, element_bytes):
            raw_left = int.from_bytes(left[offset : offset + element_bytes], "little")
            raw_right = int.from_bytes(
                right[offset : offset + element_bytes],
                "little",
            )
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
        element_bytes, signed, _saturating, floating_format = self._mode_format(
            tile_formats.TMUL, tile_formats.TMUL_MAC
        )
        left = self._memory.read_bytes(self.source0, TILE_BYTES)
        right = self._memory.read_bytes(self.source1, TILE_BYTES)
        existing = self._memory.read_bytes(self.destination, TILE_BYTES)
        bits = element_bytes * 8
        lane_mask = (1 << bits) - 1
        output = bytearray(TILE_BYTES)

        if floating_format is not None:
            self._memory.write_bytes(
                self.destination,
                tile_float.pack_lanes(
                    floating_format,
                    tile_float.fused_multiply_add(
                        floating_format,
                        tile_float.unpack_lanes(floating_format, left),
                        tile_float.unpack_lanes(floating_format, right),
                        tile_float.unpack_lanes(floating_format, existing),
                    ),
                ),
            )
            self._account()
            return

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
        element_bytes, signed, _saturating, floating_format = self._mode_format(
            tile_formats.TRED, _REDUCTION_FUNCTIONS[operation]
        )
        tile = self._memory.read_bytes(self.source0, TILE_BYTES)
        raw_values = [
            int.from_bytes(tile[offset : offset + element_bytes], "little")
            for offset in range(0, TILE_BYTES, element_bytes)
        ]

        if floating_format is not None:
            if operation == "sum":
                self._publish_float_sum(
                    floating_format,
                    tile_float.sum_lanes(floating_format, raw_values),
                )
            elif operation == "sum_squares":
                self._publish_float_sum(
                    floating_format,
                    tile_float.sum_squares(floating_format, raw_values),
                )
            elif operation in ("minimum", "maximum"):
                largest = operation == "maximum"
                self._publish_float_extreme(
                    floating_format,
                    tile_float.extreme(floating_format, raw_values, largest),
                    largest,
                )
            else:  # pragma: no cover - private callers constrain this value
                raise AssertionError(f"unknown tile reduction {operation!r}")
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

        if operation in ("minimum", "maximum"):
            self._publish_integer_extreme(
                result,
                signed=signed,
                largest=operation == "maximum",
            )
        else:
            self._publish_integer_reduction(result)
        self._account()

    def _index_reduce(self, *, minimum: bool) -> None:
        element_bytes, signed, _saturating, floating_format = self._mode_format(
            tile_formats.TRED,
            tile_formats.TRED_MINIDX if minimum else tile_formats.TRED_MAXIDX,
        )
        tile = self._memory.read_bytes(self.source0, TILE_BYTES)
        raw_values = [
            int.from_bytes(tile[offset : offset + element_bytes], "little")
            for offset in range(0, TILE_BYTES, element_bytes)
        ]

        if floating_format is not None:
            wide = ieee_fp.accumulation_format(floating_format)
            index, value = tile_float.extreme_index(
                floating_format, raw_values, not minimum
            )
            _zero, accumulate = self._take_accumulator_controls()
            words = list(self.accumulator)
            if accumulate and not tile_float.index_replaces(
                wide, value, words[1] & wide.mask, not minimum
            ):
                index, value = words[0], words[1]
            self._registers.replace_accumulator_words(
                self._core_id,
                (index, value, 0, 0),
            )
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

    def _publish_integer_extreme(
        self,
        result: int,
        *,
        signed: bool,
        largest: bool,
    ) -> None:
        """Integer MIN/MAX keep a running extreme against ACC0 (§4.6)."""

        _zero, accumulate = self._take_accumulator_controls()
        if accumulate:
            old = self.accumulator[0]
            if signed:
                old = self._signed(old, 64)
            result = max(old, result) if largest else min(old, result)
        result &= _ACCUMULATOR_MASK
        self._registers.replace_accumulator_words(
            self._core_id,
            tuple(
                (result >> (index * 64)) & MASK64
                for index in range(ACCUMULATOR_WORDS)
            ),
        )

    def _publish_float_sum(self, floating_format, result: int) -> None:
        """Publish DOT, SUM, SUMSQ, or L1 (docs/floating-point.md §4.4)."""

        wide = ieee_fp.accumulation_format(floating_format)
        _zero, accumulate = self._take_accumulator_controls()
        if accumulate:
            result = tile_float.accumulate_sum(
                wide, self.accumulator[0] & wide.mask, result
            )
        self._registers.replace_accumulator_words(
            self._core_id,
            (result, 0, 0, 0),
        )

    def _publish_float_extreme(
        self,
        floating_format,
        result: int,
        largest: bool,
    ) -> None:
        """Publish TRED MIN or MAX (docs/floating-point.md §4.4)."""

        wide = ieee_fp.accumulation_format(floating_format)
        _zero, accumulate = self._take_accumulator_controls()
        if accumulate:
            result = tile_float.accumulate_extreme(
                wide, self.accumulator[0] & wide.mask, result, largest
            )
        self._registers.replace_accumulator_words(
            self._core_id,
            (result, 0, 0, 0),
        )

    def _take_accumulator_controls(self) -> tuple[bool, bool]:
        """Consume ACC_ZERO; it takes priority over ACC_ACC."""

        zero = bool(self._control & 0x02)
        if zero:
            self._control &= ~0x02
        return zero, bool(self._control & 0x01) and not zero

    def _mode_format(
        self,
        op: int,
        funct: int,
    ) -> tuple[int, bool, bool, ieee_fp.Format | None]:
        lane_format = tile_formats.decode(self._mode)
        if not tile_formats.admits(lane_format, op, funct):
            raise UnsupportedTileModeError(self._mode)
        if lane_format.is_float:
            return lane_format.lane_bytes, False, False, lane_format.float_format
        return (
            lane_format.lane_bytes,
            bool(self._mode & tile_formats.TMODE_SIGNED),
            bool(self._mode & tile_formats.TMODE_SATURATE),
            None,
        )

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
