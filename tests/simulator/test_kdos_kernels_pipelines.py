"""Contiguous unchanged-source acceptance for KDOS kernels and pipelines."""

from __future__ import annotations

import hashlib
from pathlib import Path
import re

import pytest

from shared.fp import FP16_FORMAT, fp32_to_bits
from simulator.errors import ExecutionError, SourceError, StepBudgetExceeded
from simulator.runtime import MegaForthRuntime
from simulator.stacks import StackUnderflow
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_buffer_fp import (
    _decode_float_tile,
    _float_tile,
    _load_buffer_fp,
)
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-kernels-pipelines-3217-3754.f"
)

FIRST_LINE = 3217
LAST_LINE = 3754
SLICE_SHA256 = (
    "ec724b8ca6f6887a2c4ce724edf9612726cf04a48416c29c2eb3ed9448949e40"
)
SLICE_GIT_BLOB = "118d11a7f4dfc594612f0a3b3c2d87f697292b35"
NEXT_SEAM_SHA256 = (
    "2c35db4a06a324409fb142afd84c2d8a59b5877fe2c721e0e6e960d803b06a9b"
)
NEXT_SEAM_GIT_BLOB = "c272534c08d711af11930474f3cf670277d343ca"

DEFINITIONS = (
    b"KERN-COUNT",
    b"KERN-TABLE",
    b"K.IN",
    b"K.OUT",
    b"K.FOOT",
    b"K.FLAGS",
    b"KDESC",
    b"KERNEL",
    b"K.INFO",
    b"KERNELS",
    b"kzero",
    b"kzero-desc",
    b"kfill",
    b"kfill-desc",
    b"kadd",
    b"kadd-desc",
    b"ksum",
    b"ksum-desc",
    b"kstats",
    b"kstats-desc",
    b"kscale",
    b"kscale-desc",
    b"kthresh",
    b"kthresh-desc",
    b"kclamp",
    b"kclamp-desc",
    b"mavg-scratch",
    b"MAVG-SUM",
    b"MAVG-WIN",
    b"MAVG-NBYTES",
    b"kavg",
    b"kavg-desc",
    b"hist-bins",
    b"khistogram",
    b"khistogram-desc",
    b"HIST@",
    b".HIST",
    b"DELTA-PREV",
    b"kdelta",
    b"kdelta-desc",
    b"NORM-MIN",
    b"NORM-MAX",
    b"NORM-RANGE",
    b"knorm",
    b"knorm-desc",
    b"kpeak",
    b"kpeak-desc",
    b"RMS-SSQ",
    b"krms",
    b"krms-buf",
    b"krms-desc",
    b"kcorrelate",
    b"kcorrelate-desc",
    b"conv-scratch",
    b"CONV-C0",
    b"CONV-C1",
    b"CONV-C2",
    b"CONV-DESC",
    b"kconvolve3",
    b"kconvolve3-desc",
    b"kinvert",
    b"kinvert-desc",
    b"KCOUNT-N",
    b"kcount",
    b"kcount-desc",
    b"kfsum",
    b"kfsum-desc",
    b"kfdot",
    b"kfdot-desc",
    b"kfsumsq",
    b"kfsumsq-desc",
    b"kfadd",
    b"kfadd-desc",
    b"kfmul",
    b"kfmul-desc",
    b"PIPE-COUNT",
    b"PIPE-TABLE",
    b"P.CAP",
    b"P.COUNT",
    b"P.DATA",
    b"PDESC",
    b"P-XT",
    b"P-PIPE",
    b"PIPELINE",
    b"P.GET",
    b"P.SET",
    b"P.ADD",
    b"P.CLEAR",
    b"P.RUN",
    b"BENCH-T0",
    b"BENCH",
    b".BENCH",
    b"P.BENCH",
    b"P.INFO",
    b"PIPES",
    b"demo-a",
    b"demo-b",
    b"demo-c",
    b"p1-fill",
    b"p1-sum",
    b"pipe-fill-sum",
    b"p2-init",
    b"p2-add",
    b"p2-stats",
    b"pipe-add-stats",
    b"p3-fill",
    b"p3-thresh",
    b"p3-stats",
    b"pipe-thresh",
)

KERNEL_METADATA = (
    (b"kzero-desc", (1, 1, 0, 0)),
    (b"kfill-desc", (1, 1, 0, 0)),
    (b"kadd-desc", (2, 1, 3, 1)),
    (b"ksum-desc", (1, 0, 1, 1)),
    (b"kstats-desc", (1, 0, 3, 1)),
    (b"kscale-desc", (1, 1, 1, 0)),
    (b"kthresh-desc", (1, 1, 1, 0)),
    (b"kclamp-desc", (1, 1, 1, 0)),
    (b"kavg-desc", (1, 1, 5, 0)),
    (b"khistogram-desc", (1, 1, 4, 0)),
    (b"kdelta-desc", (1, 1, 2, 0)),
    (b"knorm-desc", (1, 1, 2, 1)),
    (b"kpeak-desc", (1, 1, 2, 0)),
    (b"krms-desc", (1, 0, 1, 0)),
    (b"kcorrelate-desc", (2, 0, 2, 1)),
    (b"kconvolve3-desc", (1, 1, 5, 0)),
    (b"kinvert-desc", (1, 1, 1, 0)),
    (b"kcount-desc", (1, 0, 1, 0)),
    (b"kfsum-desc", (1, 0, 1, 1)),
    (b"kfdot-desc", (2, 0, 1, 1)),
    (b"kfsumsq-desc", (1, 0, 1, 1)),
    (b"kfadd-desc", (2, 1, 3, 1)),
    (b"kfmul-desc", (2, 1, 3, 1)),
)

PIPELINES = (
    (b"pipe-fill-sum", 2, (b"p1-fill", b"p1-sum")),
    (b"pipe-add-stats", 3, (b"p2-init", b"p2-add", b"p2-stats")),
    (b"pipe-thresh", 3, (b"p3-fill", b"p3-thresh", b"p3-stats")),
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 16_586
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    0 TMODE! ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_kernels_pipelines(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_kernels_pipelines(
    runtime: MegaForthRuntime | None = None,
) -> MegaForthRuntime:
    return _evaluate_kernels_pipelines(_load_buffer_fp(runtime))


@pytest.fixture
def loaded_kernels_pipelines() -> MegaForthRuntime:
    return _load_kernels_pipelines()


def _descriptor(runtime: MegaForthRuntime, address: int) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(address + offset) for offset in range(0, 32, 8)
    )


def _variable(runtime: MegaForthRuntime, name: bytes | str) -> int:
    return runtime.memory.read64(_execute(runtime, name)[0])


def _define_buffer(
    runtime: MegaForthRuntime,
    name: str,
    *,
    width: int = 1,
    length: int = 64,
) -> tuple[int, int]:
    runtime.evaluate(
        f"0 {width} {length} BUFFER {name}".encode("ascii"),
        source_name=f"{name.lower()}-buffer",
    )
    descriptor = _execute(runtime, name)[0]
    return descriptor, _execute(runtime, "B.DATA", descriptor)[0]


def test_kernel_pipeline_slice_is_exact_and_loads_its_complete_state(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    assert len(DEFINITIONS) == 109
    for name in DEFINITIONS:
        assert runtime.find(name) is not None

    assert _variable(runtime, "KERN-COUNT") == 23
    assert _variable(runtime, "PIPE-COUNT") == 3
    assert _variable(runtime, "BUF-COUNT") == 6
    kernel_table = _execute(runtime, "KERN-TABLE")[0]
    for index, (name, metadata) in enumerate(KERNEL_METADATA):
        descriptor = _execute(runtime, name)[0]
        assert runtime.memory.read64(kernel_table + index * 8) == descriptor
        assert _descriptor(runtime, descriptor) == metadata

    expected_buffers = (
        b"demo-c",
        b"demo-b",
        b"demo-a",
        b"conv-scratch",
        b"hist-bins",
        b"mavg-scratch",
    )
    assert tuple(_execute(runtime, "BUF-NTH", index)[0] for index in range(6)) == (
        tuple(_execute(runtime, name)[0] for name in expected_buffers)
    )
    assert _descriptor(runtime, _execute(runtime, "mavg-scratch")[0])[:3] == (
        0,
        1,
        256,
    )
    assert _descriptor(runtime, _execute(runtime, "hist-bins")[0])[:3] == (
        0,
        8,
        256,
    )
    assert _descriptor(runtime, _execute(runtime, "conv-scratch")[0])[:3] == (
        0,
        1,
        256,
    )

    pipeline_table = _execute(runtime, "PIPE-TABLE")[0]
    for index, (name, capacity, steps) in enumerate(PIPELINES):
        descriptor = _execute(runtime, name)[0]
        assert runtime.memory.read64(pipeline_table + index * 8) == descriptor
        assert _execute(runtime, "P.CAP", descriptor) == (capacity,)
        assert _execute(runtime, "P.COUNT", descriptor) == (len(steps),)
        assert tuple(
            _execute(runtime, "P.GET", descriptor, step)[0]
            for step in range(len(steps))
        ) == tuple(runtime.find(step).xt for step in steps)  # type: ignore[union-attr]
    assert runtime.drain_uart_output() == b""


def test_next_contiguous_storage_source_stops_at_disk_status(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[LAST_LINE:3771])
    assert len(next_source) == 698
    assert next_source.count(b"\n") == 17
    assert hashlib.sha256(next_source).hexdigest() == NEXT_SEAM_SHA256
    assert _git_blob_id(next_source) == NEXT_SEAM_GIT_BLOB
    here_before = runtime.dictionary.here

    with pytest.raises(SourceError, match="unknown word b'DISK@'") as caught:
        runtime.evaluate(
            next_source,
            source_name=f"kdos.f@{MEGAPAD_REVISION}:3755-3771",
        )

    assert caught.value.location.line == 17
    assert caught.value.location.column == 22
    assert _execute(runtime, "SECTOR") == (512,)
    assert runtime.find("DISK?") is None
    assert runtime.dictionary.latest_word is runtime.find("SECTOR")
    assert runtime.dictionary.here > here_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_kernel_and_pipeline_registries_silently_omit_overflow_definitions(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    kernel_table = _execute(runtime, "KERN-TABLE")[0]
    extra_kernels = []
    for index in range(10):
        name = f"overflow-kernel-{index}"
        runtime.evaluate(f"0 0 0 0 KERNEL {name}".encode("ascii"))
        extra_kernels.append(_execute(runtime, name)[0])
    assert _variable(runtime, "KERN-COUNT") == 32
    assert runtime.memory.read64(kernel_table + 31 * 8) == extra_kernels[-2]
    assert extra_kernels[-1] not in tuple(
        runtime.memory.read64(kernel_table + index * 8) for index in range(32)
    )
    assert _descriptor(runtime, extra_kernels[-1]) == (0, 0, 0, 0)

    pipeline_table = _execute(runtime, "PIPE-TABLE")[0]
    extra_pipelines = []
    for index in range(6):
        name = f"overflow-pipeline-{index}"
        runtime.evaluate(f"0 PIPELINE {name}".encode("ascii"))
        extra_pipelines.append(_execute(runtime, name)[0])
    assert _variable(runtime, "PIPE-COUNT") == 8
    assert runtime.memory.read64(pipeline_table + 7 * 8) == extra_pipelines[-2]
    assert extra_pipelines[-1] not in tuple(
        runtime.memory.read64(pipeline_table + index * 8) for index in range(8)
    )
    assert _execute(runtime, "P.CAP", extra_pipelines[-1]) == (0,)
    assert _execute(runtime, "P.COUNT", extra_pipelines[-1]) == (0,)


def test_basic_byte_kernels_execute_their_ordinary_buffer_paths(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    source, source_data = _define_buffer(runtime, "K-BASIC-SOURCE")
    right, right_data = _define_buffer(runtime, "K-BASIC-RIGHT")
    destination, destination_data = _define_buffer(runtime, "K-BASIC-DEST")

    assert _execute(runtime, "kfill", 7, source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == bytes((7,)) * 64
    assert _execute(runtime, "kzero", source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == bytes(64)

    ramp = bytes(range(64))
    runtime.memory.write_bytes(source_data, ramp)
    runtime.memory.write_bytes(right_data, bytes((2,)) * 64)
    assert _execute(runtime, "kadd", source, right, destination) == ()
    assert runtime.memory.read_bytes(destination_data, 64) == bytes(
        value + 2 for value in ramp
    )
    assert _execute(runtime, "ksum", source) == (sum(ramp),)
    assert _execute(runtime, "kstats", source) == (sum(ramp), 0, 63)

    assert _execute(runtime, "kscale", 3, source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == bytes(
        (value * 3) & 0xFF for value in ramp
    )
    runtime.memory.write_bytes(source_data, ramp)
    assert _execute(runtime, "kthresh", 32, source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == (
        bytes(32) + bytes((255,)) * 32
    )

    runtime.memory.write_bytes(source_data, ramp)
    assert _execute(runtime, "kclamp", 10, 20, source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == bytes(
        max(10, min(20, value)) for value in ramp
    )
    runtime.memory.write_bytes(source_data, ramp)
    assert _execute(runtime, "kinvert", source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == bytes(
        255 - value for value in ramp
    )
    runtime.memory.write_bytes(source_data, ramp)
    assert _execute(runtime, "kcount", 7, source) == (1,)


def test_stateful_sample_kernels_pin_current_algorithms_and_defects(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    source, source_data = _define_buffer(runtime, "K-STATE-SOURCE")
    destination, destination_data = _define_buffer(runtime, "K-STATE-DEST")
    ramp = bytes(range(64))

    runtime.memory.write_bytes(source_data, ramp)
    assert _execute(runtime, "kavg", 9, source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == ramp
    assert _variable(runtime, "MAVG-WIN") == 9
    assert _variable(runtime, "MAVG-NBYTES") == 64
    assert _variable(runtime, "MAVG-SUM") == 0

    assert _execute(runtime, "khistogram", source) == ()
    assert _execute(runtime, "HIST@", 0) == (1,)
    assert _execute(runtime, "HIST@", 63) == (1,)
    assert _execute(runtime, "HIST@", 64) == (0,)
    assert _execute(runtime, "khistogram", source) == ()
    assert _execute(runtime, "HIST@", 0) == (1,)
    assert _execute(runtime, "HIST@", 63) == (1,)
    assert _execute(runtime, "HIST@", 64) == (0,)

    delta_source = bytes((10, 13, 12, 255)) + bytes((255,)) * 60
    runtime.memory.write_bytes(source_data, delta_source)
    assert _execute(runtime, "kdelta", source, destination) == ()
    previous = 0
    expected_delta = bytearray()
    for value in delta_source:
        expected_delta.append((value - previous) & 0xFF)
        previous = value
    assert expected_delta[0] == 10
    assert runtime.memory.read_bytes(destination_data, 64) == expected_delta

    normalization_source = bytes(range(10, 74))
    runtime.memory.write_bytes(source_data, normalization_source)
    assert _execute(runtime, "knorm", source) == ()
    assert runtime.memory.read_bytes(source_data, 64) == bytes(
        (value - 10) * 255 // 63 for value in normalization_source
    )

    peak_source = bytes((1, 5, 1, 4, 1)) + bytes(59)
    runtime.memory.write_bytes(source_data, peak_source)
    runtime.memory.write_bytes(destination_data, bytes((0xAA,)) * 64)
    assert _execute(runtime, "kpeak", 4, source, destination) == ()
    assert runtime.memory.read_bytes(source_data, 64) == peak_source
    assert runtime.memory.read_bytes(destination_data, 64) == (
        bytes((0, 255, 0, 255, 0)) + bytes(59)
    )


def test_short_kpeak_zeroes_destination_then_underflows_its_cleanup(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    source, source_data = _define_buffer(runtime, "K-SHORT-SOURCE", length=2)
    destination, destination_data = _define_buffer(
        runtime,
        "K-SHORT-DEST",
        length=2,
    )
    runtime.memory.write_bytes(source_data, b"\x01\x05")
    runtime.memory.write_bytes(destination_data, b"\xAA\xBB")
    context = runtime.main_context
    for value in (4, source, destination):
        context.data.push(value)

    with pytest.raises(StackUnderflow, match="data stack underflow"):
        runtime.execute("kpeak")

    assert runtime.memory.read_bytes(source_data, 2) == b"\x01\x05"
    assert runtime.memory.read_bytes(destination_data, 2) == bytes(2)
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_rms_correlation_and_convolution_keep_the_source_algorithms(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    left, left_data = _define_buffer(runtime, "K-MATH-LEFT")
    right, right_data = _define_buffer(runtime, "K-MATH-RIGHT")

    runtime.memory.write_bytes(left_data, bytes((4,)) * 64)
    assert _execute(runtime, "krms-buf", left) == (4,)
    runtime.memory.write_bytes(left_data, bytes((3,)) * 64)
    runtime.memory.write_bytes(right_data, bytes((4,)) * 64)
    assert _execute(runtime, "kcorrelate", left, right) == (768,)

    convolution, convolution_data = _define_buffer(
        runtime,
        "K-MATH-CONVOLUTION",
        length=5,
    )
    runtime.memory.write_bytes(convolution_data, bytes((0, 0, 255, 0, 0)))
    assert _execute(runtime, "kconvolve3", 64, 128, 64, convolution) == ()
    assert runtime.memory.read_bytes(convolution_data, 5) == bytes((0, 63, 127, 63, 0))

    runtime.memory.write_bytes(left_data, bytes((1,)) * 64)
    context = runtime.main_context
    context.data.push(left)
    with pytest.raises(ExecutionError, match="signed division trapped"):
        runtime.execute("krms-buf")
    assert context.returns.snapshot() == ()
    context.data.clear()
    assert _variable(runtime, "RMS-SSQ") == 64


def test_original_krms_loses_its_descriptor_before_dividing(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    descriptor, data = _define_buffer(runtime, "K-BROKEN-RMS", length=2)
    runtime.memory.write_bytes(data, bytes((3, 4)))
    context = runtime.main_context
    context.data.push(descriptor)

    with pytest.raises(ExecutionError, match="signed division trapped"):
        runtime.execute("krms")

    assert context.returns.snapshot() == ()
    context.data.clear()
    assert _variable(runtime, "RMS-SSQ") == 25


def test_fp16_kernel_wrappers_use_the_existing_buffer_tile_path(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    left, left_data = _define_buffer(runtime, "K-FP-LEFT", width=2, length=32)
    right, right_data = _define_buffer(runtime, "K-FP-RIGHT", width=2, length=32)
    destination, destination_data = _define_buffer(
        runtime,
        "K-FP-DEST",
        width=2,
        length=32,
    )
    runtime.memory.write_bytes(
        left_data,
        _float_tile(FP16_FORMAT, (1.0, 2.0, 3.0, 4.0)),
    )
    runtime.memory.write_bytes(
        right_data,
        _float_tile(FP16_FORMAT, (2.0, 2.0, 2.0, 2.0)),
    )

    assert _execute(runtime, "kfsum", left) == (fp32_to_bits(10.0),)
    assert _execute(runtime, "kfsumsq", left) == (fp32_to_bits(30.0),)
    assert _execute(runtime, "kfdot", left, right) == (fp32_to_bits(20.0),)
    assert _execute(runtime, "kfadd", left, right, destination) == ()
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data, 64),
    )[:4] == (3.0, 4.0, 5.0, 6.0)
    assert _execute(runtime, "kfmul", left, right, destination) == ()
    assert _decode_float_tile(
        FP16_FORMAT,
        runtime.memory.read_bytes(destination_data, 64),
    )[:4] == (2.0, 4.0, 6.0, 8.0)


def test_demo_pipelines_run_ordinary_bound_steps_and_emit_results(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    expected = (
        (b"pipe-fill-sum", b" sum=2688 \r\n"),
        (b"pipe-add-stats", b" max=30   min=30   sum=1920 \r\n"),
        (b"pipe-thresh", b" max=255   min=0   sum=8160 \r\n"),
    )
    for name, output in expected:
        descriptor = _execute(runtime, name)[0]
        assert _execute(runtime, "P.RUN", descriptor) == ()
        assert runtime.drain_uart_output() == output

    demo_a = _execute(runtime, "demo-a")[0]
    demo_a_data = _execute(runtime, "B.DATA", demo_a)[0]
    assert runtime.memory.read_bytes(demo_a_data, 64) == (
        bytes(32) + bytes((255,)) * 32
    )


def test_pipeline_capacity_clear_and_benchmark_semantics_are_source_visible(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    pipeline = _execute(runtime, "pipe-fill-sum")[0]
    original_steps = (
        _execute(runtime, "P.GET", pipeline, 0)[0],
        _execute(runtime, "P.GET", pipeline, 1)[0],
    )
    ignored_xt = runtime.find("p2-init").xt  # type: ignore[union-attr]

    assert _execute(runtime, "P.ADD", ignored_xt, pipeline) == ()
    assert _execute(runtime, "P.COUNT", pipeline) == (2,)
    assert tuple(
        _execute(runtime, "P.GET", pipeline, index)[0] for index in range(2)
    ) == original_steps
    assert _execute(runtime, "P.CLEAR", pipeline) == ()
    assert _execute(runtime, "P.COUNT", pipeline) == (0,)
    assert tuple(
        _execute(runtime, "P.GET", pipeline, index)[0] for index in range(2)
    ) == original_steps

    for step in original_steps:
        assert _execute(runtime, "P.ADD", step, pipeline) == ()
    assert _execute(runtime, "P.COUNT", pipeline) == (2,)

    fill_xt = runtime.find("p1-fill").xt  # type: ignore[union-attr]
    first = _execute(runtime, "BENCH", fill_xt)[0]
    second = _execute(runtime, "BENCH", fill_xt)[0]
    assert first == second
    assert first > 0

    pipeline = _execute(runtime, "pipe-fill-sum")[0]
    assert _execute(runtime, "P.BENCH", pipeline) == ()
    output = runtime.drain_uart_output()
    normalized = re.sub(rb"= [0-9]+  cycles", b"= <HOST-WORK> cycles", output)
    assert normalized == (
        b" Pipeline (2  steps):\r\n"
        b"   step 0  = <HOST-WORK> cycles\r\n"
        b" sum=2688 \r\n"
        b"   step 1  = <HOST-WORK> cycles\r\n"
    )


def test_zero_length_kernel_loop_does_not_complete_normally(
    loaded_kernels_pipelines: MegaForthRuntime,
) -> None:
    runtime = loaded_kernels_pipelines
    descriptor, _data = _define_buffer(runtime, "K-ZERO-COUNT", length=0)
    context = runtime.main_context
    context.data.push(0)
    context.data.push(descriptor)

    with pytest.raises(StepBudgetExceeded):
        runtime.execute("kcount", step_budget=80)

    assert runtime.main_context.returns.snapshot() == ()
    context.data.clear()
