"""Contiguous unchanged-source acceptance for byte tile Buffer operations."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from asm import assemble
from emulator.megapad64 import Megapad64 as PythonMegapad64
from shared.cells import MASK64
from simulator.errors import StepBudgetExceeded
from simulator.memory import CrossRegionAccessError
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.tile import UnsupportedTileModeError
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_buffer import _load_buffer
from tests.simulator.test_kdos_hbw import CANONICAL_HBW_SIZE
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-buffer-tile-2986-3109.f"
)

FIRST_LINE = 2986
LAST_LINE = 3109
SLICE_SHA256 = (
    "91d0fc5a15da85c31f9e4c4fcf17691c2bd32ba306b6b5bc338a7cf8b1ab96c4"
)
SLICE_GIT_BLOB = "4b15984477ff8ae1d9fb22f0810ffc158e4594c7"
DEFINITIONS = (
    b"B.SUM",
    b"B.MIN",
    b"B.MAX",
    b"BTMP-NTILES",
    b"B.ADD",
    b"B.SUB",
    b"B.SCALE",
)

SOURCE0 = 0x2B_000
SOURCE1 = 0x2B_100
DESTINATION = 0x2B_200


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 4_170
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    AB-DESC @ CONSTANT ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_buffer_tile(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}"
        ),
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_buffer_tile(
    runtime: MegaForthRuntime | None = None,
) -> MegaForthRuntime:
    return _evaluate_buffer_tile(_load_buffer(runtime))


@pytest.fixture
def loaded_buffer_tile() -> MegaForthRuntime:
    return _load_buffer_tile(
        MegaForthRuntime(
            memory=create_one_core_address_space(
                hbw_size=CANONICAL_HBW_SIZE,
            )
        )
    )


def _define_hbw_buffer(
    runtime: MegaForthRuntime,
    name: str,
    length: int,
) -> tuple[int, int]:
    runtime.evaluate(
        f"0 1 {length} HBW-BUFFER {name}".encode("ascii"),
        source_name=f"{name.lower()}-buffer",
    )
    descriptor = _execute(runtime, name)[0]
    return descriptor, _execute(runtime, "B.DATA", descriptor)[0]


def _lane_tile(width: int, values: tuple[int, ...]) -> bytes:
    mask = (1 << (width * 8)) - 1
    lanes = 64 // width
    return b"".join(
        (values[index % len(values)] & mask).to_bytes(width, "little")
        for index in range(lanes)
    )


def test_next_contiguous_fp_buffer_slice_is_now_admitted(
    loaded_buffer_tile: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_tile
    for name in DEFINITIONS:
        assert runtime.find(name) is not None
    assert runtime.tile.mode == 0
    assert runtime.tile.control == 0
    assert runtime.tile.accumulator == (0, 0, 0, 0)

    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[LAST_LINE:3216])
    assert next_source.count(b"\n") == 107
    assert next_source.startswith(b"\n\\ ====")
    assert next_source.endswith(b"    0 TMODE! ;\n")

    result = runtime.evaluate(
        next_source,
        source_name=f"kdos.f@{MEGAPAD_REVISION}:3110-3216",
    )

    assert tuple(word.name for word in result.definitions) == (
        b"F.SUM",
        b"F.DOT",
        b"F.SUMSQ",
        b"F.ADD",
        b"F.MUL",
        b"BF.SUM",
        b"BF.DOT",
    )
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_bios_u8_add_sub_latch_exact_state_and_count_completed_operations() -> None:
    runtime = MegaForthRuntime()
    left = bytes((index * 7 + 250) & 0xFF for index in range(64))
    right = bytes((index * 11 + 10) & 0xFF for index in range(64))
    runtime.memory.write_bytes(SOURCE0, left)
    runtime.memory.write_bytes(SOURCE1, right)
    runtime.memory.write_bytes(DESTINATION, bytes((0xA5,)) * 64)
    runtime.field.replace_accumulator_words(0, (1, 2, 3, 4))

    runtime.evaluate(
        (
            f"256 TMODE! 258 TCTRL! {SOURCE0} TSRC0! "
            f"{SOURCE1} TSRC1! {DESTINATION} TDST! TADD"
        ).encode("ascii"),
        source_name="tile-add",
    )
    assert runtime.memory.read_bytes(DESTINATION, 64) == bytes(
        (a + b) & 0xFF for a, b in zip(left, right)
    )
    assert runtime.tile.mode == 0
    assert runtime.tile.control == 2
    assert runtime.tile.source0 == SOURCE0
    assert runtime.tile.source1 == SOURCE1
    assert runtime.tile.destination == DESTINATION
    assert runtime.tile.accumulator == (1, 2, 3, 4)
    assert runtime.field.operand_address(0) == SOURCE0
    assert runtime.field.result_address(0) == DESTINATION
    assert runtime.diagnostics.perf_tileops == 1

    runtime.evaluate(b"TSUB", source_name="tile-subtract")
    assert runtime.memory.read_bytes(DESTINATION, 64) == bytes(
        (a - b) & 0xFF for a, b in zip(left, right)
    )
    assert runtime.tile.control == 2
    assert runtime.tile.accumulator == (1, 2, 3, 4)
    assert runtime.diagnostics.perf_tileops == 2

    unaligned_source0 = SOURCE0 + 3
    unaligned_source1 = SOURCE1 + 5
    unaligned_destination = DESTINATION + 7
    runtime.memory.write_bytes(unaligned_source0, bytes((40,)) * 64)
    runtime.memory.write_bytes(unaligned_source1, bytes((2,)) * 64)
    runtime.tile.set_source0(unaligned_source0)
    runtime.tile.set_source1(unaligned_source1)
    runtime.tile.set_destination(unaligned_destination)
    runtime.tile.add()
    assert runtime.memory.read_bytes(unaligned_destination, 64) == bytes((42,)) * 64
    assert runtime.tile.source0 == unaligned_source0
    assert runtime.tile.source1 == unaligned_source1
    assert runtime.tile.destination == unaligned_destination
    assert runtime.diagnostics.perf_tileops == 3


def test_integer_widths_signed_saturation_and_reserved_formats_fail_closed() -> None:
    runtime = MegaForthRuntime()
    vectors = (
        (0x01, 2, (0xFFFF, 1), (2, 3), (1, 4), (0xFFFD, 0xFFFE)),
        (
            0x31,
            2,
            (32760, -32760),
            (100, 100),
            (32767, -32660),
            (32660, -32768),
        ),
        (
            0x22,
            4,
            (0xFFFF_FFF0, 5),
            (0x30, 10),
            (0xFFFF_FFFF, 15),
            (0xFFFF_FFC0, 0),
        ),
        (
            0x03,
            8,
            (MASK64 - 1, 3),
            (5, 7),
            (3, 10),
            (MASK64 - 6, MASK64 - 3),
        ),
    )

    for mode, width, left, right, added, subtracted in vectors:
        runtime.memory.write_bytes(SOURCE0, _lane_tile(width, left))
        runtime.memory.write_bytes(SOURCE1, _lane_tile(width, right))
        runtime.tile.set_mode(mode)
        runtime.tile.set_source0(SOURCE0)
        runtime.tile.set_source1(SOURCE1)
        runtime.tile.set_destination(DESTINATION)
        runtime.tile.add()
        assert runtime.memory.read_bytes(DESTINATION, 64) == _lane_tile(
            width,
            added,
        )
        runtime.tile.subtract()
        assert runtime.memory.read_bytes(DESTINATION, 64) == _lane_tile(
            width,
            subtracted,
        )

    runtime.memory.write_bytes(SOURCE0, _lane_tile(2, (-32768, 1234)))
    runtime.tile.set_mode(0x11)
    runtime.tile.set_control(2)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.minimum()
    assert runtime.tile.accumulator == (
        (1 << 64) - 32768,
        MASK64,
        MASK64,
        MASK64,
    )
    runtime.tile.set_control(2)
    runtime.tile.maximum()
    assert runtime.tile.accumulator == (1234, 0, 0, 0)

    before = runtime.memory.read_bytes(DESTINATION, 64)
    operations = runtime.diagnostics.perf_tileops
    for reserved_mode in (6, 7):
        runtime.tile.set_mode(reserved_mode)
        with pytest.raises(
            UnsupportedTileModeError,
            match=f"tile mode 0x{reserved_mode:02x}",
        ):
            runtime.tile.add()
    assert runtime.memory.read_bytes(DESTINATION, 64) == before
    assert runtime.diagnostics.perf_tileops == operations


@pytest.mark.parametrize(
    ("mode", "width", "left_values", "right_values"),
    (
        pytest.param(0x00, 1, (250, 1, 255), (10, 2, 1), id="u8-wrap"),
        pytest.param(
            0x31,
            2,
            (32760, -32760, -1),
            (100, 100, 2),
            id="i16-saturate",
        ),
        pytest.param(
            0x22,
            4,
            (0xFFFF_FFF0, 5),
            (0x30, 10),
            id="u32-saturate",
        ),
        pytest.param(
            0x13,
            8,
            (MASK64 - 1, 3),
            (5, 7),
            id="i64-wrap",
        ),
    ),
)
def test_hosted_integer_tiles_match_decoded_architectural_emulator(
    mode: int,
    width: int,
    left_values: tuple[int, ...],
    right_values: tuple[int, ...],
) -> None:
    source0, source1, destination = 0x100, 0x140, 0x180
    left = _lane_tile(width, left_values)
    right = _lane_tile(width, right_values)
    sentinel = bytes((0xA5,)) * 64
    program = assemble(
        "t.add\nt.sub\nt.mul\nt.dot\nt.sum\nt.rmin\nt.rmax\nt.sumsq"
    )
    assert len(program) == 16

    emulator = PythonMegapad64(mem_size=0x400)
    emulator.load_bytes(0, program)
    emulator.pc = 0
    emulator.mem[source0 : source0 + 64] = left
    emulator.mem[source1 : source1 + 64] = right
    emulator.mem[destination : destination + 64] = sentinel
    emulator.tmode = mode
    emulator.tctrl = 3
    emulator.tsrc0 = source0
    emulator.tsrc1 = source1
    emulator.tdst = destination
    initial_accumulator = (11, 22, 33, 44)
    emulator.acc = list(initial_accumulator)

    runtime = MegaForthRuntime()
    runtime.memory.write_bytes(source0, left)
    runtime.memory.write_bytes(source1, right)
    runtime.memory.write_bytes(destination, sentinel)
    runtime.tile.set_mode(mode)
    runtime.tile.set_control(3)
    runtime.tile.set_source0(source0)
    runtime.tile.set_source1(source1)
    runtime.tile.set_destination(destination)
    runtime.field.replace_accumulator_words(0, initial_accumulator)

    hosted_operations = (
        runtime.tile.add,
        runtime.tile.subtract,
        runtime.tile.multiply,
        runtime.tile.dot,
        runtime.tile.sum,
        runtime.tile.minimum,
        runtime.tile.maximum,
        runtime.tile.sum_squares,
    )
    for hosted_operation in hosted_operations:
        emulator.step()
        hosted_operation()
        assert runtime.memory.read_bytes(destination, 64) == bytes(
            emulator.mem[destination : destination + 64]
        )
        assert runtime.tile.accumulator == tuple(emulator.acc)
        assert runtime.tile.control == emulator.tctrl

    assert emulator.pc == len(program)
    assert runtime.diagnostics.perf_tileops == len(hosted_operations)


def test_reductions_share_acc_consume_zero_and_retain_accumulate_control() -> None:
    runtime = MegaForthRuntime()
    first = bytes(range(64))
    second = bytes((2,)) * 64
    extrema = bytes((5, 250)) + bytes(range(6, 68))
    runtime.memory.write_bytes(SOURCE0, first)
    runtime.memory.write_bytes(SOURCE1, second)
    runtime.memory.write_bytes(DESTINATION, extrema)
    runtime.field.replace_accumulator_words(0, (99, 88, 77, 66))

    runtime.evaluate(
        f"0 TMODE! 2 TCTRL! {SOURCE0} TSRC0! TSUM".encode("ascii"),
        source_name="tile-sum-first",
    )
    assert runtime.tile.accumulator == (2016, 0, 0, 0)
    assert runtime.field.accumulator_words(0) == (2016, 0, 0, 0)
    assert runtime.tile.control == 0

    runtime.evaluate(
        f"1 TCTRL! {SOURCE1} TSRC0! TSUM".encode("ascii"),
        source_name="tile-sum-second",
    )
    assert _execute(runtime, "ACC@") == (2144,)
    assert runtime.tile.accumulator == (2144, 0, 0, 0)
    assert runtime.tile.control == 1

    runtime.evaluate(
        f"2 TCTRL! {DESTINATION} TSRC0! TMIN".encode("ascii"),
        source_name="tile-minimum",
    )
    assert _execute(runtime, "ACC@") == (5,)
    assert runtime.tile.control == 0
    runtime.evaluate(b"2 TCTRL! TMAX", source_name="tile-maximum")
    assert _execute(runtime, "ACC@") == (250,)
    assert runtime.tile.accumulator == (250, 0, 0, 0)
    assert runtime.diagnostics.perf_tileops == 4


def test_tile_faults_preserve_destination_accumulator_control_and_counter() -> None:
    runtime = MegaForthRuntime()
    bank0 = runtime.memory.regions[0]
    destination = bank0.limit - 32
    runtime.memory.write_bytes(SOURCE0, bytes((1,)) * 64)
    runtime.memory.write_bytes(SOURCE1, bytes((2,)) * 64)
    runtime.memory.write_bytes(destination, bytes((0xA5,)) * 32)
    runtime.field.replace_accumulator_words(0, (11, 22, 33, 44))
    runtime.tile.set_mode(0)
    runtime.tile.set_control(3)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.set_source1(SOURCE1)
    runtime.tile.set_destination(destination)

    with pytest.raises(CrossRegionAccessError):
        runtime.tile.add()

    assert runtime.memory.read_bytes(destination, 32) == bytes((0xA5,)) * 32
    assert runtime.tile.accumulator == (11, 22, 33, 44)
    assert runtime.tile.control == 3
    assert runtime.diagnostics.perf_tileops == 0


def test_b_sum_accumulates_full_tiles_and_includes_partial_tile_tail(
    loaded_buffer_tile: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_tile
    full, full_data = _define_hbw_buffer(runtime, "SUM-FULL", 128)
    first = bytes(range(64))
    second = bytes((2,)) * 64
    runtime.memory.write_bytes(full_data, first + second)

    assert _execute(runtime, "B.SUM", full) == (2144,)
    assert runtime.tile.mode == 0
    assert runtime.tile.control == 1
    assert runtime.tile.source0 == full_data + 64
    assert runtime.tile.accumulator == (2144, 0, 0, 0)

    partial, partial_data = _define_hbw_buffer(runtime, "SUM-PARTIAL", 1)
    runtime.memory.write_bytes(partial_data, bytes((5,)) + bytes((1,)) * 63)
    assert _execute(runtime, "B.SUM", partial) == (68,)


def test_b_min_max_pin_safe_empty_one_tile_and_multitile_address_bug(
    loaded_buffer_tile: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_tile
    empty, _empty_data = _define_hbw_buffer(runtime, "EXTREMA-EMPTY", 0)
    runtime.tile.set_control(0xA5)
    assert _execute(runtime, "B.MIN", empty) == (0,)
    assert _execute(runtime, "B.MAX", empty) == (0,)
    assert runtime.tile.control == 0xA5

    one, one_data = _define_hbw_buffer(runtime, "EXTREMA-ONE", 1)
    runtime.memory.write_bytes(one_data, bytes((4, 250)) + bytes((9,)) * 62)
    assert _execute(runtime, "B.MIN", one) == (4,)
    assert _execute(runtime, "B.MAX", one) == (250,)

    minimum, minimum_data = _define_hbw_buffer(runtime, "MIN-TWO", 128)
    runtime.memory.write_bytes(minimum_data, bytes((64,)) * 64 + bytes((1,)) * 64)
    runtime.memory.write_bytes(64, bytes((7,)) * 64)
    assert _execute(runtime, "B.MIN", minimum) == (7,)
    assert runtime.tile.source0 == 64

    maximum, maximum_data = _define_hbw_buffer(runtime, "MAX-TWO", 128)
    runtime.memory.write_bytes(
        maximum_data,
        bytes((128,)) * 64 + bytes((200,)) * 64,
    )
    runtime.memory.write_bytes(128, bytes((250,)) * 64)
    assert _execute(runtime, "B.MAX", maximum) == (250,)
    assert runtime.tile.source0 == 128


def test_b_add_sub_process_complete_tiles_and_overwrite_partial_tail(
    loaded_buffer_tile: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_tile
    left, left_data = _define_hbw_buffer(runtime, "ADD-LEFT", 128)
    right, right_data = _define_hbw_buffer(runtime, "ADD-RIGHT", 128)
    destination, destination_data = _define_hbw_buffer(runtime, "ADD-DEST", 128)
    left_bytes = bytes((250,)) * 64 + bytes(range(64))
    right_bytes = bytes((10,)) * 64 + bytes(reversed(range(64)))
    runtime.memory.write_bytes(left_data, left_bytes)
    runtime.memory.write_bytes(right_data, right_bytes)

    assert _execute(runtime, "B.ADD", left, right, destination) == ()
    assert runtime.memory.read_bytes(destination_data, 128) == bytes(
        (a + b) & 0xFF for a, b in zip(left_bytes, right_bytes)
    )
    assert runtime.tile.source0 == left_data + 64
    assert runtime.tile.source1 == right_data + 64
    assert runtime.tile.destination == destination_data + 64

    assert _execute(runtime, "B.SUB", left, right, destination) == ()
    assert runtime.memory.read_bytes(destination_data, 128) == bytes(
        (a - b) & 0xFF for a, b in zip(left_bytes, right_bytes)
    )
    scratch = _execute(runtime, "BTMP-NTILES")[0]
    assert runtime.memory.read64(scratch) == 2

    runtime.memory.write64(right + 16, 1)
    runtime.memory.write64(destination + 16, 1)
    runtime.memory.write_bytes(destination_data, bytes((0xA5,)) * 128)
    assert _execute(runtime, "B.ADD", left, right, destination) == ()
    assert runtime.memory.read_bytes(destination_data, 128) == bytes(
        (a + b) & 0xFF for a, b in zip(left_bytes, right_bytes)
    )
    assert _execute(runtime, "B.TILES", right) == (1,)
    assert _execute(runtime, "B.TILES", destination) == (1,)

    short_left, short_left_data = _define_hbw_buffer(runtime, "SHORT-LEFT", 1)
    short_right, short_right_data = _define_hbw_buffer(runtime, "SHORT-RIGHT", 1)
    short_dest, short_dest_data = _define_hbw_buffer(runtime, "SHORT-DEST", 1)
    runtime.memory.write_bytes(short_left_data, bytes(range(64)))
    runtime.memory.write_bytes(short_right_data, bytes((3,)) * 64)
    runtime.memory.write_bytes(short_dest_data, bytes((0xA5,)) * 64)

    assert _execute(runtime, "B.ADD", short_left, short_right, short_dest) == ()
    assert runtime.memory.read_bytes(short_dest_data, 64) == bytes(
        (value + 3) & 0xFF for value in range(64)
    )
    assert _execute(runtime, "B.SUB", short_left, short_right, short_dest) == ()
    assert runtime.memory.read_bytes(short_dest_data, 64) == bytes(
        (value - 3) & 0xFF for value in range(64)
    )


def test_b_scale_wraps_exact_logical_bytes_without_touching_tile_state(
    loaded_buffer_tile: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_tile
    descriptor, data = _define_hbw_buffer(runtime, "SCALE-BUF", 3)
    runtime.memory.write_bytes(data, bytes((128, 200, 255)) + bytes((0xA5,)) * 61)
    runtime.tile.set_mode(0x13)
    runtime.tile.set_control(0xA5)
    runtime.tile.set_source0(0x1111)
    runtime.tile.set_source1(0x2222)
    runtime.tile.set_destination(0x3333)
    runtime.field.replace_accumulator_words(0, (1, 2, 3, 4))
    before = (
        runtime.tile.mode,
        runtime.tile.control,
        runtime.tile.source0,
        runtime.tile.source1,
        runtime.tile.destination,
        runtime.tile.accumulator,
        runtime.diagnostics.perf_tileops,
    )

    assert _execute(runtime, "B.SCALE", 2, descriptor) == ()
    assert runtime.memory.read_bytes(data, 64) == bytes((0, 144, 254)) + bytes(
        (0xA5,)
    ) * 61
    assert (
        runtime.tile.mode,
        runtime.tile.control,
        runtime.tile.source0,
        runtime.tile.source1,
        runtime.tile.destination,
        runtime.tile.accumulator,
        runtime.diagnostics.perf_tileops,
    ) == before


def test_zero_length_b_sum_is_not_zero_trip_safe(
    loaded_buffer_tile: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_tile
    descriptor, data = _define_hbw_buffer(runtime, "SUM-EMPTY", 0)
    runtime.memory.write_bytes(data, bytes((1,)) * 64)
    context = runtime.main_context
    context.data.push(descriptor)

    with pytest.raises(StepBudgetExceeded):
        runtime.execute("B.SUM", step_budget=80)

    assert runtime.diagnostics.perf_tileops > 0
    context.data.clear()
    context.returns.clear()
