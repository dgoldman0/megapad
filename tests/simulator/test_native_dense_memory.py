"""Native dense leases and execution share the ordinary guest byte image."""

from __future__ import annotations

from array import array
import gc

import pytest


native = pytest.importorskip("_megaforth_native")

from shared.cells import MASK64  # noqa: E402
from shared.memory_backing import DenseMemoryBacking  # noqa: E402
from simulator.memory import EXTERNAL_BASE, UnmappedAddressError  # noqa: E402
from simulator.platform import create_one_core_address_space  # noqa: E402
from simulator.runtime import MegaForthRuntime  # noqa: E402
from simulator.stacks import Continuation  # noqa: E402
from tests.simulator.test_native_execution import _observe  # noqa: E402


def _runtimes(source=b"", *, page_size=4096, external_size=8197):
    runtimes = []
    for backend, dense in (("python", False), ("python", True),
                           ("native", False), ("native", True)):
        memory = create_one_core_address_space(
            page_size=page_size, external_size=external_size,
            dense_backing=True if dense else None,
        )
        runtime = MegaForthRuntime(memory=memory, execution_backend=backend)
        if source:
            runtime.evaluate(source, source_name="dense-memory.f")
        runtimes.append(runtime)
    return runtimes


def _parity(runtimes, *, require_native=True, native_steps=None, **kwargs):
    results = [_observe(runtime, "RUN", **kwargs) for runtime in runtimes]
    assert all(result[0] == results[0][0] for result in results[1:])
    assert results[0][1] == results[1][1] == 0
    if require_native:
        assert results[2][1] > 0 and results[3][1] > 0
    if native_steps is not None:
        assert results[2][1] == results[3][1] == native_steps
    assert len({runtime.scalar_float.fpcsr for runtime in runtimes}) == 1
    assert len({runtime.timer.counter for runtime in runtimes}) == 1
    return results[0][0]


@pytest.mark.parametrize("page_size", [16, 4096])
def test_dense_stacks_nested_calls_loops_branches_and_fp_match_sparse(page_size):
    runtimes = _runtimes(
        b": INNER 19 >R R@ R> + ; "
        b": RUN 0 12 0 DO I + LOOP INNER + DUP 100 > IF 1+ THEN "
        b"1 S>F64 3 S>F64 F64/ FPCSR@ ;",
        page_size=page_size,
    )
    observed = _parity(runtimes)
    assert observed["error"] is None
    assert observed["data"] == (105, 0x3FD5555555555555, 1 << 4)
    assert observed["returns"] == ()
    assert observed["result_steps"] == observed["counted_steps"]


@pytest.mark.parametrize("operation", ["MOVE", "CMOVE", "CMOVE>", "FILL", "COMPARE"])
def test_dense_bulk_primitives_execute_natively_with_identical_overlap(operation):
    runtimes = _runtimes(b": RUN " + operation.encode("ascii") + b" ;",
                         page_size=16)
    first, second = EXTERNAL_BASE + 4093, EXTERNAL_BASE + 5007
    payload = bytes(range(0x41, 0x41 + 37))
    for runtime in runtimes:
        runtime.memory.write_bytes(first, payload)
        runtime.memory.write_bytes(second, payload[:-1] + b"\xFF")
    arguments = {
        "MOVE": (first, second, 37),
        "CMOVE": (first, first + 1, 36),
        "CMOVE>": (first + 1, first, 36),
        "FILL": (first, 37, 0x1A5),
        "COMPARE": (first, 37, second, 37),
    }[operation]
    observed = _parity(runtimes, inputs=arguments,
                       spans=((first, 37), (second, 37)), native_steps=2)
    assert observed["error"] is None
    assert observed["data"] == ((MASK64,) if operation == "COMPARE" else ())
    if operation == "MOVE":
        assert observed["memory"][1] == payload
    elif operation == "CMOVE":
        assert observed["memory"][0] == payload[:1] * 37
    elif operation == "CMOVE>":
        assert observed["memory"][0] == payload[-1:] * 37
    elif operation == "FILL":
        assert observed["memory"][0] == b"\xA5" * 37


def test_dense_bulk_fallback_keeps_partial_writes_at_region_end():
    runtimes = _runtimes(b": RUN CMOVE ;", external_size=37)
    for runtime in runtimes:
        runtime.memory.write_bytes(EXTERNAL_BASE, b"ABCDEFGH")
    observed = _parity(runtimes, inputs=(EXTERNAL_BASE, EXTERNAL_BASE + 33, 8),
                       spans=((EXTERNAL_BASE + 33, 4),), require_native=False,
                       native_steps=0)
    assert observed["error"][0] is UnmappedAddressError
    assert observed["memory"] == (b"ABCD",)
    assert observed["data"] == ()
    assert observed["counted_steps"] == 2


def test_host_mutation_and_native_stores_use_the_same_unaligned_bytes():
    runtimes = _runtimes()
    offset = 4093
    for runtime in runtimes:
        runtime.define_constant("BUFFER", EXTERNAL_BASE + offset)
        runtime.evaluate(b": RUN BUFFER @ 1+ DUP BUFFER ! ;")
    for value in (13, 55):
        for runtime in runtimes:
            runtime.main_context.data.clear()
            owner = runtime.memory.dense_backing
            if owner is None:
                runtime.memory.write64(EXTERNAL_BASE + offset, value)
            else:
                view = owner.buffer_at(EXTERNAL_BASE)
                view[offset:offset + 8] = value.to_bytes(8, "little")
                view.release()
        observed = _parity(runtimes, spans=((EXTERNAL_BASE + offset, 8),))
        assert observed["error"] is None
        assert observed["data"] == (value + 1,)
        assert observed["memory"] == ((value + 1).to_bytes(8, "little"),)
        for runtime in (runtimes[1], runtimes[3]):
            view = runtime.memory.dense_backing.buffer_at(EXTERNAL_BASE)
            assert int.from_bytes(view[offset:offset + 8], "little") == value + 1
            view.release()


def test_native_dense_lease_survives_client_release_and_pins_until_destruction():
    exporter = bytearray(256)
    caller_view = memoryview(exporter)
    program = native.NativeProgram([], 64, Continuation,
                                   dense_regions=[(0, 256, caller_view)])
    caller_view.release()
    program.install(1, [(native.OP_LITERAL, 17, 0), (native.OP_STOP, 0, 0)])
    result = program.run(1, 0, (0, 128, 128), (128, 256, 256, 0), {}, 10)
    assert result[:5] == (1, 1, 1, 120, 256)
    assert int.from_bytes(exporter[120:128], "little") == 17
    exporter[120:128] = (21).to_bytes(8, "little")
    assert program.snapshot_stack((0, 128, 120)) == (21,)
    program.install(1, [(native.OP_ONE_PLUS, 0, 0), (native.OP_STOP, 0, 0)])
    assert program.run(1, 0, (0, 128, 120), (128, 256, 256, 0), {}, 10)[:3] == (1, 1, 2)
    assert int.from_bytes(exporter[120:128], "little") == 22
    with pytest.raises(BufferError):
        exporter.append(0)
    del program
    gc.collect()
    exporter.append(0)
    assert len(exporter) == 257


def test_native_lease_keeps_owner_bytes_alive_after_python_owner_is_dropped():
    owner = DenseMemoryBacking(((0, 256),))
    caller_view = owner.buffer_at(0)
    caller_view[120:128] = (91).to_bytes(8, "little")
    program = native.NativeProgram([], 64, Continuation,
                                   dense_regions=[(0, 256, caller_view)])
    caller_view.release()
    del caller_view, owner
    gc.collect()
    assert program.snapshot_stack((0, 128, 120)) == (91,)


@pytest.mark.parametrize("factory,size,error", [
    (lambda: bytes(32), 32, (BufferError, ValueError)),
    (lambda: memoryview(bytearray(64))[::2], 32, ValueError),
    (lambda: memoryview(bytearray(32))[::-1], 32, ValueError),
    (lambda: memoryview(bytearray(32)).cast("B", shape=(4, 8)), 32, ValueError),
    (lambda: array("Q", [0] * 4), 32, ValueError),
    (lambda: bytearray(31), 32, ValueError),
    (lambda: bytearray(33), 32, ValueError),
])
def test_native_rejects_buffers_without_exact_writable_byte_geometry(factory, size, error):
    buffer = factory()
    before = bytes(buffer)
    with pytest.raises(error):
        native.NativeProgram([], 16, Continuation,
                             dense_regions=[(0, size, buffer)])
    assert bytes(buffer) == before


@pytest.mark.parametrize("mixed_sparse", [False, True])
def test_native_rejects_overlapping_guest_regions(mixed_sparse):
    sparse = [(0, 32, {})] if mixed_sparse else []
    dense = [] if mixed_sparse else [(0, 32, bytearray(32))]
    dense.append((16, 32, bytearray(32)))
    with pytest.raises(ValueError, match="overlap"):
        native.NativeProgram(sparse, 16, Continuation, dense_regions=dense)


def test_native_rejects_physical_aliases_even_at_disjoint_guest_addresses():
    exporter = bytearray(48)
    first = memoryview(exporter)[:32]
    second = memoryview(exporter)[16:]
    with pytest.raises(ValueError, match="alias"):
        native.NativeProgram([], 16, Continuation,
                             dense_regions=[(0, 32, first), (128, 32, second)])


def test_adjacent_host_slices_are_valid_distinct_dense_regions():
    exporter = bytearray(64)
    first = memoryview(exporter)[:32]
    second = memoryview(exporter)[32:]
    program = native.NativeProgram([], 16, Continuation,
                                   dense_regions=[(0, 32, first), (128, 32, second)])
    first.release()
    second.release()
    exporter[24:32] = (17).to_bytes(8, "little")
    exporter[56:64] = (91).to_bytes(8, "little")
    assert program.snapshot_stack((0, 32, 24)) == (17,)
    assert program.snapshot_stack((128, 160, 152)) == (91,)


def test_failed_native_construction_releases_previously_acquired_pins():
    first = bytearray(32)
    with pytest.raises((BufferError, ValueError)):
        native.NativeProgram([], 16, Continuation,
                             dense_regions=[(0, 32, first), (128, 32, bytes(32))])
    first.extend(b"x")
    assert len(first) == 33


def test_native_can_resolve_disjoint_sparse_and_dense_regions_together():
    pages = {index: bytearray(64) for index in range(4)}
    dense = bytearray(64)
    dense[:8] = (37).to_bytes(8, "little")
    program = native.NativeProgram([(0, 256, pages)], 64, Continuation,
                                   dense_regions=[(0x1000, 64, dense)])
    program.install(1, [(native.OP_PUSH_CELL, 0x1000, 0),
                        (native.OP_FETCH, 0, 0), (native.OP_STOP, 0, 0)])
    result = program.run(1, 0, (0, 128, 128), (128, 256, 256, 0), {}, 10)
    assert result[:5] == (1, 2, 4, 120, 256)
    assert program.snapshot_stack((0, 128, 120)) == (37,)
    assert int.from_bytes(pages[1][56:64], "little") == 37
