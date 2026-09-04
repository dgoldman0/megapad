"""Contiguous unchanged-source acceptance for FP16/BF16 Buffer operations."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from asm import assemble
from emulator.megapad64 import Megapad64 as PythonMegapad64
from shared.fp import (
    BF16_FORMAT,
    FP16_FORMAT,
    decode_tile_float,
    encode_tile_float,
    fp16_to_float,
    fp32_to_bits,
)
from simulator.errors import StepBudgetExceeded
from simulator.memory import CrossRegionAccessError, HBW_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_buffer_tile import _load_buffer_tile
from tests.simulator.test_kdos_hbw import CANONICAL_HBW_SIZE
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-buffer-fp-3110-3216.f"

FIRST_LINE = 3110
LAST_LINE = 3216
SLICE_SHA256 = (
    "cea60476207e132760c32cf2fb82773d6325d6d1895f0e7d73c40bf667b75065"
)
SLICE_GIT_BLOB = "5fdcbc62aca939fb620e47386ae8a50c18b99969"
DEFINITIONS = (
    b"F.SUM",
    b"F.DOT",
    b"F.SUMSQ",
    b"F.ADD",
    b"F.MUL",
    b"BF.SUM",
    b"BF.DOT",
)

SOURCE0 = 0x2C_000
SOURCE1 = 0x2C_100
DESTINATION = 0x2C_200


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 2_869
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    2DROP ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_buffer_fp(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_buffer_fp(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_buffer_fp(_load_buffer_tile(runtime))


@pytest.fixture
def loaded_buffer_fp() -> MegaForthRuntime:
    return _load_buffer_fp(
        MegaForthRuntime(
            memory=create_one_core_address_space(
                hbw_size=CANONICAL_HBW_SIZE,
            )
        )
    )


def _raw_float_tile(values: tuple[int, ...]) -> bytes:
    return b"".join(
        values[index % len(values)].to_bytes(2, "little")
        for index in range(32)
    )


def _float_tile(format_code: int, values: tuple[float, ...]) -> bytes:
    padded = values + (0.0,) * (32 - len(values))
    assert len(padded) == 32
    return b"".join(
        encode_tile_float(value, format_code).to_bytes(2, "little")
        for value in padded
    )


def _decode_float_tile(format_code: int, tile: bytes) -> tuple[float, ...]:
    return tuple(
        decode_tile_float(
            int.from_bytes(tile[offset : offset + 2], "little"),
            format_code,
        )
        for offset in range(0, 64, 2)
    )


def _define_fp_buffer(
    runtime: MegaForthRuntime,
    name: str,
    *,
    elements: int = 32,
) -> tuple[int, int]:
    runtime.evaluate(
        f"0 2 {elements} HBW-BUFFER {name}".encode("ascii"),
        source_name=f"{name.lower()}-fp-buffer",
    )
    descriptor = _execute(runtime, name)[0]
    return descriptor, _execute(runtime, "B.DATA", descriptor)[0]


def test_next_contiguous_kernel_pipeline_slice_is_now_admitted(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    for name in DEFINITIONS:
        assert runtime.find(name) is not None
    assert runtime.tile.mode == 0
    assert runtime.tile.control == 0

    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[LAST_LINE:3754])
    assert len(next_source) == 16_586
    assert next_source.count(b"\n") == 538
    assert next_source.startswith(b"\n\\ ====")
    assert next_source.endswith(b"' p3-stats  pipe-thresh P.ADD\n")

    result = runtime.evaluate(
        next_source,
        source_name=f"kdos.f@{MEGAPAD_REVISION}:3217-3754",
    )

    assert len(result.definitions) == 109
    assert result.definitions[0].name == b"KERN-COUNT"
    assert result.definitions[-1].name == b"pipe-thresh"
    for name in ("KERNEL", "P.CLEAR", "BENCH", "pipe-thresh"):
        assert runtime.find(name) is not None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


@pytest.mark.parametrize(
    ("format_code", "left_values", "right_values"),
    (
        pytest.param(
            FP16_FORMAT,
            (0x3C00, 0xC000, 0x0001, 0x8000, 0x4300),
            (0x4000, 0x3800, 0x3C00, 0x0000, 0xBC00),
            id="fp16",
        ),
        pytest.param(
            BF16_FORMAT,
            (0x3F80, 0xC000, 0x0001, 0x8000, 0x4060),
            (0x4000, 0x3F00, 0x3F80, 0x0000, 0xBF80),
            id="bf16",
        ),
    ),
)
def test_hosted_fp_tiles_match_decoded_architectural_emulator(
    format_code: int,
    left_values: tuple[int, ...],
    right_values: tuple[int, ...],
) -> None:
    source0, source1, destination = 0x100, 0x140, 0x180
    left = _raw_float_tile(left_values)
    right = _raw_float_tile(right_values)
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
    emulator.tmode = format_code | 0x30
    emulator.tctrl = 3
    emulator.tsrc0 = source0
    emulator.tsrc1 = source1
    emulator.tdst = destination
    emulator.acc = [0x3F80_0000, 22, 33, 44]

    runtime = MegaForthRuntime()
    runtime.memory.write_bytes(source0, left)
    runtime.memory.write_bytes(source1, right)
    runtime.memory.write_bytes(destination, sentinel)
    runtime.tile.set_mode(format_code | 0x30)
    runtime.tile.set_control(3)
    runtime.tile.set_source0(source0)
    runtime.tile.set_source1(source1)
    runtime.tile.set_destination(destination)
    runtime.field.replace_accumulator_words(0, emulator.acc)

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


def test_fp16_multiply_preserves_the_executable_subnormal_carry_behavior() -> None:
    runtime = MegaForthRuntime()
    left = (0x0017).to_bytes(2, "little") * 32
    right = (0x5190).to_bytes(2, "little") * 32
    runtime.memory.write_bytes(SOURCE0, left)
    runtime.memory.write_bytes(SOURCE1, right)
    runtime.tile.set_mode(FP16_FORMAT)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.set_source1(SOURCE1)
    runtime.tile.set_destination(DESTINATION)

    runtime.tile.multiply()

    emulator = PythonMegapad64(mem_size=0x400)
    emulator.load_bytes(0, assemble("t.mul"))
    emulator.mem[0x100:0x140] = left
    emulator.mem[0x140:0x180] = right
    emulator.tmode = FP16_FORMAT
    emulator.tsrc0 = 0x100
    emulator.tsrc1 = 0x140
    emulator.tdst = 0x180
    emulator.step()

    assert fp16_to_float(0x0017) * fp16_to_float(0x5190) == 2.0**-14 - 2.0**-25
    assert runtime.memory.read_bytes(DESTINATION, 64) == bytes(64)
    assert bytes(emulator.mem[0x180:0x1C0]) == bytes(64)


def test_fp16_sum_uses_the_pinned_python_builtin_sum_oracle() -> None:
    runtime = MegaForthRuntime()
    source = _float_tile(
        FP16_FORMAT,
        (65504.0,) + (2.0**-24,) * 30 + (-65504.0,),
    )
    runtime.memory.write_bytes(SOURCE0, source)
    runtime.tile.set_mode(FP16_FORMAT)
    runtime.tile.set_control(2)
    runtime.tile.set_source0(SOURCE0)

    runtime.tile.sum()

    emulator = PythonMegapad64(mem_size=0x400)
    emulator.load_bytes(0, assemble("t.sum"))
    emulator.mem[0x100:0x140] = source
    emulator.tmode = FP16_FORMAT
    emulator.tctrl = 2
    emulator.tsrc0 = 0x100
    emulator.tsrc1 = 0x140
    emulator.step()

    assert runtime.tile.accumulator == tuple(emulator.acc)
    assert runtime.tile.accumulator[1:] == (0, 0, 0)
    assert runtime.tile.control == 0


@pytest.mark.parametrize(
    ("format_code", "nan_raw", "negative_raw", "positive_raw"),
    (
        pytest.param(FP16_FORMAT, 0x7E01, 0xC000, 0x4200, id="fp16"),
        pytest.param(BF16_FORMAT, 0x7FC1, 0xC000, 0x4040, id="bf16"),
    ),
)
def test_fp_extrema_skip_nan_and_ignore_accumulate(
    format_code: int,
    nan_raw: int,
    negative_raw: int,
    positive_raw: int,
) -> None:
    runtime = MegaForthRuntime()
    runtime.memory.write_bytes(
        SOURCE0,
        _raw_float_tile((nan_raw, negative_raw, positive_raw)),
    )
    runtime.tile.set_mode(format_code)
    runtime.tile.set_control(1)
    runtime.tile.set_source0(SOURCE0)
    runtime.field.replace_accumulator_words(
        0,
        (fp32_to_bits(100.0), 22, 33, 44),
    )

    runtime.tile.minimum()
    assert runtime.tile.accumulator == (fp32_to_bits(-2.0), 0, 0, 0)
    runtime.tile.maximum()
    assert runtime.tile.accumulator == (fp32_to_bits(3.0), 0, 0, 0)

    runtime.memory.write_bytes(SOURCE0, nan_raw.to_bytes(2, "little") * 32)
    runtime.tile.maximum()
    assert runtime.tile.accumulator[0] & 0x7F80_0000 == 0x7F80_0000
    assert runtime.tile.accumulator[0] & 0x007F_FFFF
    assert runtime.tile.accumulator[1:] == (0, 0, 0)
    assert runtime.tile.control == 1


@pytest.mark.parametrize(
    ("operation", "invalid_register"),
    (
        pytest.param("multiply", "destination", id="multiply-destination"),
        pytest.param("dot", "source1", id="dot-source1"),
        pytest.param("sum_squares", "source0", id="sumsq-source0"),
    ),
)
def test_new_tile_operations_fail_before_publishing_state(
    operation: str,
    invalid_register: str,
) -> None:
    runtime = MegaForthRuntime()
    bank0 = runtime.memory.regions[0]
    invalid = bank0.limit - 32
    runtime.memory.write_bytes(SOURCE0, _float_tile(FP16_FORMAT, (1.0,)))
    runtime.memory.write_bytes(SOURCE1, _float_tile(FP16_FORMAT, (2.0,)))
    runtime.memory.write_bytes(DESTINATION, bytes((0xA5,)) * 64)
    runtime.tile.set_mode(FP16_FORMAT)
    runtime.tile.set_control(3)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.set_source1(SOURCE1)
    runtime.tile.set_destination(DESTINATION)
    runtime.field.replace_accumulator_words(0, (11, 22, 33, 44))
    getattr(runtime.tile, f"set_{invalid_register}")(invalid)

    with pytest.raises(CrossRegionAccessError):
        getattr(runtime.tile, operation)()

    assert runtime.memory.read_bytes(DESTINATION, 64) == bytes((0xA5,)) * 64
    assert runtime.tile.accumulator == (11, 22, 33, 44)
    assert runtime.tile.control == 3
    assert runtime.diagnostics.perf_tileops == 0


def test_fp16_buffer_words_return_raw_fp32_and_restore_default_mode(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    left, left_data = _define_fp_buffer(runtime, "FP-LEFT")
    right, right_data = _define_fp_buffer(runtime, "FP-RIGHT")
    destination, destination_data = _define_fp_buffer(runtime, "FP-DEST")
    left_values = (1.0, 2.0, 3.0, 4.0)
    right_values = (2.0, 2.0, 2.0, 2.0)
    runtime.memory.write_bytes(left_data, _float_tile(FP16_FORMAT, left_values))
    runtime.memory.write_bytes(right_data, _float_tile(FP16_FORMAT, right_values))

    assert _execute(runtime, "F.ADD", left, right, destination) == ()
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data, 64),
    )[:4] == (3.0, 4.0, 5.0, 6.0)
    assert _execute(runtime, "F.MUL", left, right, destination) == ()
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data, 64),
    )[:4] == (2.0, 4.0, 6.0, 8.0)

    assert _execute(runtime, "F.SUM", left) == (fp32_to_bits(10.0),)
    assert _execute(runtime, "F.SUMSQ", left) == (fp32_to_bits(30.0),)
    assert _execute(runtime, "F.DOT", left, right) == (fp32_to_bits(20.0),)
    assert runtime.tile.mode == 0
    assert runtime.tile.control == 1
    assert runtime.tile.accumulator == (fp32_to_bits(20.0), 0, 0, 0)
    assert runtime.diagnostics.perf_tileops == 5


def test_bf16_buffer_words_use_bfloat_lanes_and_return_raw_fp32(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    left, left_data = _define_fp_buffer(runtime, "BF-LEFT")
    right, right_data = _define_fp_buffer(runtime, "BF-RIGHT")
    left_values = (1.0, 2.0, 4.0, 8.0)
    right_values = (0.5, 0.5, 0.5, 0.5)
    runtime.memory.write_bytes(left_data, _float_tile(BF16_FORMAT, left_values))
    runtime.memory.write_bytes(right_data, _float_tile(BF16_FORMAT, right_values))

    assert _execute(runtime, "BF.SUM", left) == (fp32_to_bits(15.0),)
    assert _execute(runtime, "BF.DOT", left, right) == (fp32_to_bits(7.5),)
    assert runtime.tile.mode == 0
    assert runtime.tile.control == 1
    assert runtime.tile.accumulator == (fp32_to_bits(7.5), 0, 0, 0)
    assert runtime.diagnostics.perf_tileops == 2


def test_fp16_reductions_accumulate_two_tiles_and_retain_last_sources(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    left, left_data = _define_fp_buffer(runtime, "FP-TWO-LEFT", elements=64)
    right, right_data = _define_fp_buffer(runtime, "FP-TWO-RIGHT", elements=64)
    runtime.memory.write_bytes(
        left_data,
        _float_tile(FP16_FORMAT, (1.0,) * 32)
        + _float_tile(FP16_FORMAT, (2.0,) * 32),
    )
    runtime.memory.write_bytes(
        right_data,
        _float_tile(FP16_FORMAT, (3.0,) * 32)
        + _float_tile(FP16_FORMAT, (4.0,) * 32),
    )

    assert _execute(runtime, "F.SUM", left) == (fp32_to_bits(96.0),)
    assert runtime.tile.source0 == left_data + 64
    assert _execute(runtime, "F.SUMSQ", left) == (fp32_to_bits(160.0),)
    assert _execute(runtime, "F.DOT", left, right) == (fp32_to_bits(352.0),)
    assert runtime.tile.source0 == left_data + 64
    assert runtime.tile.source1 == right_data + 64
    assert runtime.tile.accumulator == (fp32_to_bits(352.0), 0, 0, 0)
    assert runtime.tile.control == 1
    assert runtime.tile.mode == 0
    assert runtime.diagnostics.perf_tileops == 6


def test_fp16_binary_words_trust_only_the_first_source_tile_count(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    left, left_data = _define_fp_buffer(runtime, "FP-COUNT-LEFT", elements=64)
    right, right_data = _define_fp_buffer(runtime, "FP-COUNT-RIGHT", elements=64)
    destination, destination_data = _define_fp_buffer(
        runtime,
        "FP-COUNT-DEST",
        elements=64,
    )
    runtime.memory.write64(right + 16, 32)
    runtime.memory.write64(destination + 16, 32)
    runtime.memory.write_bytes(
        left_data,
        _float_tile(FP16_FORMAT, (1.0,) * 32)
        + _float_tile(FP16_FORMAT, (2.0,) * 32),
    )
    runtime.memory.write_bytes(
        right_data,
        _float_tile(FP16_FORMAT, (10.0,) * 32) * 2,
    )
    runtime.memory.write_bytes(destination_data, bytes((0xA5,)) * 128)
    runtime.field.replace_accumulator_words(0, (11, 22, 33, 44))
    runtime.tile.set_control(0xA5)

    assert _execute(runtime, "F.ADD", left, right, destination) == ()
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data, 64),
    ) == (11.0,) * 32
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data + 64, 64),
    ) == (12.0,) * 32
    assert _execute(runtime, "F.MUL", left, right, destination) == ()
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data, 64),
    ) == (10.0,) * 32
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data + 64, 64),
    ) == (20.0,) * 32
    assert runtime.tile.source0 == left_data + 64
    assert runtime.tile.source1 == right_data + 64
    assert runtime.tile.destination == destination_data + 64
    assert runtime.tile.accumulator == (11, 22, 33, 44)
    assert runtime.tile.control == 0xA5
    assert runtime.tile.mode == 0
    assert runtime.diagnostics.perf_tileops == 4


def test_later_fp_multiply_fault_keeps_prior_tile_and_fp_mode(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    left, left_data = _define_fp_buffer(runtime, "FP-FAULT-LEFT", elements=64)
    right, right_data = _define_fp_buffer(runtime, "FP-FAULT-RIGHT", elements=64)
    destination, _destination_data = _define_fp_buffer(
        runtime,
        "FP-FAULT-DEST",
        elements=64,
    )
    runtime.memory.write_bytes(
        left_data,
        _float_tile(FP16_FORMAT, (1.0,) * 32)
        + _float_tile(FP16_FORMAT, (2.0,) * 32),
    )
    runtime.memory.write_bytes(
        right_data,
        _float_tile(FP16_FORMAT, (3.0,) * 32)
        + _float_tile(FP16_FORMAT, (4.0,) * 32),
    )
    hbw = next(region for region in runtime.memory.regions if region.base == HBW_BASE)
    crossing_base = hbw.limit - 96
    runtime.memory.write_bytes(crossing_base, bytes((0xA5,)) * 96)
    runtime.memory.write64(destination + 24, crossing_base)
    context = runtime.main_context
    for value in (left, right, destination):
        context.data.push(value)

    with pytest.raises(CrossRegionAccessError):
        runtime.execute("F.MUL", step_budget=250_000)

    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(crossing_base, 64),
    ) == (3.0,) * 32
    assert runtime.memory.read_bytes(crossing_base + 64, 32) == bytes((0xA5,)) * 32
    assert runtime.tile.mode == FP16_FORMAT
    assert runtime.tile.destination == crossing_base + 64
    assert runtime.diagnostics.perf_tileops == 1
    context.data.clear()
    context.returns.clear()


def test_fp_sum_includes_every_physical_lane_after_the_logical_tail(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    descriptor, data = _define_fp_buffer(runtime, "FP-TAIL")
    runtime.memory.write_bytes(data, _float_tile(FP16_FORMAT, (1.0,) * 32))
    runtime.memory.write64(descriptor + 16, 1)

    assert _execute(runtime, "F.SUM", descriptor) == (fp32_to_bits(32.0),)


def test_zero_length_fp_sum_enters_the_loop_and_leaves_fp_mode_on_budget_fault(
    loaded_buffer_fp: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer_fp
    descriptor, data = _define_fp_buffer(runtime, "FP-EMPTY", elements=0)
    runtime.memory.write_bytes(data, _float_tile(FP16_FORMAT, (1.0,)))
    context = runtime.main_context
    context.data.push(descriptor)

    with pytest.raises(StepBudgetExceeded):
        runtime.execute("F.SUM", step_budget=80)

    assert runtime.tile.mode == FP16_FORMAT
    assert runtime.diagnostics.perf_tileops > 0
    context.data.clear()
    context.returns.clear()
