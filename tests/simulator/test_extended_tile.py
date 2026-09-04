"""Focused executable-oracle coverage for the hosted extended tile surface."""

from __future__ import annotations

import pytest

from asm import assemble
from emulator.megapad64 import Megapad64 as PythonMegapad64
from shared.cells import MASK64
from shared.fp import BF16_FORMAT, FP16_FORMAT, encode_tile_float
from simulator.memory import MemoryAccessError
from simulator.runtime import MegaForthRuntime


SOURCE0 = 0x100
SOURCE1 = 0x140
DESTINATION = 0x180
TILE_BYTES = 64


def _lane_tile(width: int, values: tuple[int, ...]) -> bytes:
    mask = (1 << (width * 8)) - 1
    lanes = TILE_BYTES // width
    return b"".join(
        (values[index % len(values)] & mask).to_bytes(width, "little")
        for index in range(lanes)
    )


def _float_tile(format_code: int, values: tuple[float, ...]) -> bytes:
    return _lane_tile(
        2,
        tuple(encode_tile_float(value, format_code) for value in values),
    )


def _run_oracle(
    instruction: str,
    *,
    mode: int,
    source0: bytes,
    source1: bytes,
    destination: bytes,
    control: int = 0,
    accumulator: tuple[int, ...] = (11, 22, 33, 44),
) -> tuple[bytes, tuple[int, ...], int]:
    cpu = PythonMegapad64(mem_size=0x400)
    cpu.load_bytes(0, assemble(instruction))
    cpu.pc = 0
    cpu.mem[SOURCE0 : SOURCE0 + TILE_BYTES] = source0
    cpu.mem[SOURCE1 : SOURCE1 + TILE_BYTES] = source1
    cpu.mem[DESTINATION : DESTINATION + TILE_BYTES] = destination
    cpu.mem[DESTINATION + TILE_BYTES : DESTINATION + 2 * TILE_BYTES] = bytes(
        (0xC3,)
    ) * TILE_BYTES
    cpu.tmode = mode
    cpu.tctrl = control
    cpu.tsrc0 = SOURCE0
    cpu.tsrc1 = SOURCE1
    cpu.tdst = DESTINATION
    cpu.acc = list(accumulator)
    cpu.step()
    return (
        bytes(cpu.mem[DESTINATION : DESTINATION + 2 * TILE_BYTES]),
        tuple(cpu.acc),
        cpu.tctrl,
    )


def _run_hosted(
    operation: str,
    *,
    mode: int,
    source0: bytes,
    source1: bytes,
    destination: bytes,
    control: int = 0,
    accumulator: tuple[int, ...] = (11, 22, 33, 44),
) -> tuple[MegaForthRuntime, bytes, tuple[int, ...], int]:
    runtime = MegaForthRuntime()
    runtime.memory.write_bytes(SOURCE0, source0)
    runtime.memory.write_bytes(SOURCE1, source1)
    runtime.memory.write_bytes(DESTINATION, destination)
    runtime.memory.write_bytes(
        DESTINATION + TILE_BYTES,
        bytes((0xC3,)) * TILE_BYTES,
    )
    runtime.tile.set_mode(mode)
    runtime.tile.set_control(control)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.set_source1(SOURCE1)
    runtime.tile.set_destination(DESTINATION)
    runtime.field.replace_accumulator_words(0, accumulator)

    getattr(runtime.tile, operation)()

    return (
        runtime,
        runtime.memory.read_bytes(DESTINATION, 2 * TILE_BYTES),
        runtime.tile.accumulator,
        runtime.tile.control,
    )


def _assert_matches_oracle(
    operation: str,
    instruction: str,
    **state: object,
) -> MegaForthRuntime:
    expected = _run_oracle(instruction, **state)  # type: ignore[arg-type]
    runtime, *actual = _run_hosted(operation, **state)  # type: ignore[arg-type]
    assert tuple(actual) == expected
    assert runtime.diagnostics.perf_tileops == 1
    return runtime


@pytest.mark.parametrize(
    ("operation", "instruction"),
    (
        pytest.param("bitwise_and", "t.and", id="and"),
        pytest.param("bitwise_or", "t.or", id="or"),
        pytest.param("bitwise_xor", "t.xor", id="xor"),
        pytest.param("elementwise_minimum", "t.min", id="min"),
        pytest.param("elementwise_maximum", "t.max", id="max"),
        pytest.param("absolute", "t.abs", id="abs"),
    ),
)
@pytest.mark.parametrize(
    ("mode", "width", "left_values", "right_values"),
    (
        pytest.param(0x00, 1, (0, 1, 0x7F, 0x80, 0xFF), (0x55, 2), id="u8"),
        pytest.param(
            0x11,
            2,
            (-32768, -9, -1, 0, 7, 32767),
            (3, -11, 2),
            id="i16",
        ),
        pytest.param(
            0x13,
            8,
            (-(1 << 63), -3, MASK64, 17),
            (1, -7, 5),
            id="i64",
        ),
    ),
)
def test_integer_elementwise_operations_match_the_executable_machine(
    operation: str,
    instruction: str,
    mode: int,
    width: int,
    left_values: tuple[int, ...],
    right_values: tuple[int, ...],
) -> None:
    _assert_matches_oracle(
        operation,
        instruction,
        mode=mode,
        source0=_lane_tile(width, left_values),
        source1=_lane_tile(width, right_values),
        destination=bytes((0xA5,)) * TILE_BYTES,
        control=0xA3,
    )


@pytest.mark.parametrize(
    ("format_code", "nan_bits", "canonical_nan"),
    (
        pytest.param(FP16_FORMAT, 0xFD55, 0x7E00, id="fp16"),
        pytest.param(BF16_FORMAT, 0xFF95, 0x7FC0, id="bf16"),
    ),
)
def test_fp_elementwise_operations_preserve_raw_and_nan_semantics(
    format_code: int,
    nan_bits: int,
    canonical_nan: int,
) -> None:
    left = _lane_tile(2, (nan_bits, 0x8000, 0xFFFF, 0x3555))
    right = _lane_tile(2, (0x3C00, 0x0000, 0x0F0F, 0xAAAA))
    destination = bytes((0xA5,)) * TILE_BYTES
    for operation, instruction in (
        ("bitwise_and", "t.and"),
        ("bitwise_or", "t.or"),
        ("bitwise_xor", "t.xor"),
        ("elementwise_minimum", "t.min"),
        ("elementwise_maximum", "t.max"),
        ("absolute", "t.abs"),
    ):
        runtime = _assert_matches_oracle(
            operation,
            instruction,
            mode=format_code,
            source0=left,
            source1=right,
            destination=destination,
        )
        first = int.from_bytes(
            runtime.memory.read_bytes(DESTINATION, 2),
            "little",
        )
        if operation in ("elementwise_minimum", "elementwise_maximum"):
            assert first == canonical_nan
        elif operation == "absolute":
            assert first == nan_bits & 0x7FFF

    # Python min/max return the first of equal signed zeros; the executable
    # tile implementation consequently preserves the first operand's sign.
    signed_zero = _lane_tile(2, (0x8000,))
    positive_zero = _lane_tile(2, (0x0000,))
    for operation, instruction in (
        ("elementwise_minimum", "t.min"),
        ("elementwise_maximum", "t.max"),
    ):
        runtime = _assert_matches_oracle(
            operation,
            instruction,
            mode=format_code,
            source0=signed_zero,
            source1=positive_zero,
            destination=destination,
        )
        assert runtime.memory.read_bytes(DESTINATION, 2) == b"\x00\x80"


def test_transpose_is_an_in_place_byte_matrix_operation() -> None:
    original = bytes(range(TILE_BYTES))
    runtime = _assert_matches_oracle(
        "transpose",
        "t.trans",
        mode=7,
        source0=bytes((0x11,)) * TILE_BYTES,
        source1=bytes((0x22,)) * TILE_BYTES,
        destination=original,
        control=0xA3,
    )
    assert runtime.memory.read_bytes(DESTINATION, TILE_BYTES) == bytes(
        original[row * 8 + column]
        for column in range(8)
        for row in range(8)
    )


@pytest.mark.parametrize(
    ("operation", "instruction", "mode", "source"),
    (
        pytest.param(
            "popcount",
            "t.popcnt",
            0x02,
            _lane_tile(4, (0xFFFF0000, 0x01010101, 0)),
            id="popcount-u32",
        ),
        pytest.param(
            "l1_norm",
            "t.l1",
            0x11,
            _lane_tile(2, (-32768, -7, 0, 9)),
            id="l1-i16",
        ),
        pytest.param(
            "l1_norm",
            "t.l1",
            FP16_FORMAT | 0x10,
            _lane_tile(2, (0xFFFF, 0x8000, 0x7FFF, 1)),
            id="l1-fp-raw-signed",
        ),
    ),
)
def test_integer_style_reductions_match_full_accumulator_control(
    operation: str,
    instruction: str,
    mode: int,
    source: bytes,
) -> None:
    initial = (MASK64 - 8, MASK64, MASK64, MASK64)
    for control in (0, 1, 3):
        _assert_matches_oracle(
            operation,
            instruction,
            mode=mode,
            source0=source,
            source1=bytes(TILE_BYTES),
            destination=bytes((0xA5,)) * TILE_BYTES,
            control=control,
            accumulator=initial,
        )


@pytest.mark.parametrize(
    ("operation", "instruction"),
    (
        pytest.param("minimum_index", "t.minidx", id="minimum"),
        pytest.param("maximum_index", "t.maxidx", id="maximum"),
    ),
)
def test_integer_index_reductions_retain_the_running_extremum_contract(
    operation: str,
    instruction: str,
) -> None:
    source = _lane_tile(2, (-7, 4, -2, 11, 0, -13, 9, 3))
    for control, accumulator in (
        (0, (51, 52, 53, 54)),
        (1, (91, MASK64 - 9, 93, 94)),
        (3, (81, 82, 83, 84)),
    ):
        _assert_matches_oracle(
            operation,
            instruction,
            mode=0x11,
            source0=source,
            source1=bytes(TILE_BYTES),
            destination=bytes(TILE_BYTES),
            control=control,
            accumulator=accumulator,
        )


@pytest.mark.parametrize(
    ("format_code", "nan_bits"),
    (
        pytest.param(FP16_FORMAT, 0xFD55, id="fp16"),
        pytest.param(BF16_FORMAT, 0x7F95, id="bf16"),
    ),
)
def test_fp_index_reductions_skip_nan_and_ignore_accumulate(
    format_code: int,
    nan_bits: int,
) -> None:
    mixed = _lane_tile(
        2,
        (
            nan_bits,
            encode_tile_float(-2.0, format_code),
            encode_tile_float(3.0, format_code),
            nan_bits,
        ),
    )
    all_nan = _lane_tile(2, (nan_bits,))
    for operation, instruction in (
        ("minimum_index", "t.minidx"),
        ("maximum_index", "t.maxidx"),
    ):
        for source in (mixed, all_nan):
            _assert_matches_oracle(
                operation,
                instruction,
                mode=format_code,
                source0=source,
                source1=bytes(TILE_BYTES),
                destination=bytes(TILE_BYTES),
                control=1,
                accumulator=(91, 92, 93, 94),
            )


@pytest.mark.parametrize(
    ("mode", "width", "left", "right"),
    (
        pytest.param(0x00, 1, (255, 3, 17), (2, 7), id="u8"),
        pytest.param(0x11, 2, (-32768, -3, 17), (-1, 7), id="i16"),
        pytest.param(0x02, 4, (0xFFFF_FFFF, 3), (2, 7), id="u32"),
        pytest.param(
            0x13,
            8,
            (-(1 << 63), -3, 17),
            (-1, 7),
            id="i64",
        ),
    ),
)
def test_integer_widening_multiply_populates_both_destination_tiles(
    mode: int,
    width: int,
    left: tuple[int, ...],
    right: tuple[int, ...],
) -> None:
    _assert_matches_oracle(
        "widening_multiply",
        "t.wmul",
        mode=mode,
        source0=_lane_tile(width, left),
        source1=_lane_tile(width, right),
        destination=bytes((0xA5,)) * TILE_BYTES,
    )


@pytest.mark.parametrize("format_code", (FP16_FORMAT, BF16_FORMAT))
def test_fp_widening_multiply_populates_all_32_fp32_lanes(
    format_code: int,
) -> None:
    left = _float_tile(format_code, tuple(float(index + 1) for index in range(32)))
    right = _float_tile(format_code, (0.5,) * 31 + (-3.0,))
    runtime = _assert_matches_oracle(
        "widening_multiply",
        "t.wmul",
        mode=format_code,
        source0=left,
        source1=right,
        destination=bytes((0xA5,)) * TILE_BYTES,
    )
    assert runtime.memory.read_bytes(DESTINATION + 124, 4) != bytes(4)


@pytest.mark.parametrize(
    ("mode", "width", "source0", "source1", "destination"),
    (
        pytest.param(0x00, 1, (2, 3), (11, 13), (23, 29), id="u8"),
        pytest.param(0x11, 2, (-2, 3), (11, -13), (23, -29), id="i16"),
        pytest.param(0x02, 4, (2, 3), (11, 13), (23, 29), id="u32"),
        pytest.param(0x13, 8, (-2, 3), (11, -13), (23, -29), id="i64"),
    ),
)
@pytest.mark.parametrize(
    ("operation", "instruction"),
    (
        pytest.param("multiply_accumulate", "t.mac", id="mac"),
        pytest.param("fused_multiply_add", "t.fma", id="fma"),
    ),
)
def test_integer_multiply_add_operations_match_every_lane_width(
    mode: int,
    width: int,
    source0: tuple[int, ...],
    source1: tuple[int, ...],
    destination: tuple[int, ...],
    operation: str,
    instruction: str,
) -> None:
    _assert_matches_oracle(
        operation,
        instruction,
        mode=mode,
        source0=_lane_tile(width, source0),
        source1=_lane_tile(width, source1),
        destination=_lane_tile(width, destination),
        control=0xA3,
    )


@pytest.mark.parametrize("format_code", (FP16_FORMAT, BF16_FORMAT))
@pytest.mark.parametrize(
    ("operation", "instruction"),
    (
        pytest.param("multiply_accumulate", "t.mac", id="mac"),
        pytest.param("fused_multiply_add", "t.fma", id="fma"),
    ),
)
def test_fp_multiply_add_operations_match_rounding_and_nan_behavior(
    format_code: int,
    operation: str,
    instruction: str,
) -> None:
    _assert_matches_oracle(
        operation,
        instruction,
        mode=format_code,
        source0=_float_tile(format_code, (2.0, -3.0, float("nan"))),
        source1=_float_tile(format_code, (5.0, 4.0, 1.0)),
        destination=_float_tile(format_code, (1.0, 2.0, 3.0)),
    )


@pytest.mark.parametrize(
    "operation",
    (
        "multiply_accumulate",
        "minimum_index",
    ),
)
def test_extended_operations_publish_nothing_when_a_required_span_faults(
    operation: str,
) -> None:
    runtime = MegaForthRuntime()
    memory = runtime.memory
    bank0 = memory.regions[0]
    memory.write_bytes(SOURCE0, bytes((2,)) * TILE_BYTES)
    memory.write_bytes(SOURCE1, bytes((3,)) * TILE_BYTES)
    valid_destination = bank0.limit - TILE_BYTES
    memory.write_bytes(valid_destination, bytes((0xA5,)) * TILE_BYTES)
    runtime.tile.set_mode(0)
    runtime.tile.set_control(3)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.set_source1(SOURCE1)
    runtime.tile.set_destination(valid_destination)
    runtime.field.replace_accumulator_words(0, (11, 22, 33, 44))

    if operation == "multiply_accumulate":
        runtime.tile.set_destination(bank0.limit - TILE_BYTES // 2)
    elif operation == "minimum_index":
        runtime.tile.set_source0(bank0.limit - TILE_BYTES // 2)

    with pytest.raises(MemoryAccessError):
        getattr(runtime.tile, operation)()

    assert memory.read_bytes(valid_destination, TILE_BYTES) == bytes((0xA5,)) * 64
    assert runtime.tile.accumulator == (11, 22, 33, 44)
    assert runtime.tile.control == 3
    assert runtime.diagnostics.perf_tileops == 0


def test_widening_multiply_keeps_its_first_ordered_write_if_the_second_faults(
) -> None:
    runtime = MegaForthRuntime()
    memory = runtime.memory
    bank0 = memory.regions[0]
    destination = bank0.limit - TILE_BYTES
    memory.write_bytes(SOURCE0, bytes((2,)) * TILE_BYTES)
    memory.write_bytes(SOURCE1, bytes((3,)) * TILE_BYTES)
    memory.write_bytes(destination, bytes((0xA5,)) * TILE_BYTES)
    runtime.tile.set_mode(0)
    runtime.tile.set_control(3)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.set_source1(SOURCE1)
    runtime.tile.set_destination(destination)
    runtime.field.replace_accumulator_words(0, (11, 22, 33, 44))

    with pytest.raises(MemoryAccessError):
        runtime.tile.widening_multiply()

    assert memory.read_bytes(destination, TILE_BYTES) == b"\x06\x00" * 32
    assert runtime.tile.accumulator == (11, 22, 33, 44)
    assert runtime.tile.control == 3
    assert runtime.diagnostics.perf_tileops == 0
