"""Bulk native primitives preserve shared bytes and reference fault effects."""
from __future__ import annotations

import pytest

pytest.importorskip("_megaforth_native")

from shared.cells import MASK64
from simulator.memory import AddressClass, MMIO_BASE
from simulator.stacks import StackUnderflow
from tests.simulator.test_native_execution import _compare, _runtimes


def _external(runtime):
    return next(r for r in runtime.memory._regions
                if r.spec.kind is AddressClass.EXTERNAL)


@pytest.mark.parametrize("page_size", [1, 4, 16, 4096])
@pytest.mark.parametrize("left,right,expected", [
    (b"same\0bytes\xff", b"same\0bytes\xff", 0),
    (b"same\0bytes\x80", b"same\0bytes\x7f", 1),
    (b"abc", b"abd", MASK64),
    (b"prefix", b"prefix-tail", MASK64),
    (b"prefix-tail", b"prefix", 1),
    (b"", b"x", MASK64),
    (b"x", b"", 1),
    (b"", b"", 0),
])
def test_compare_unsigned_bytes_and_prefix_lengths_across_pages(page_size, left, right, expected):
    runtimes = _runtimes(b": RUN COMPARE ;", external_size=8192, page_size=page_size)
    for runtime in runtimes:
        base = _external(runtime).spec.base
        runtime.memory.write_bytes(base + 4093, left)
        runtime.memory.write_bytes(base + 7007, right)
    result = _compare(runtimes, "RUN", inputs=(base + 4093, len(left), base + 7007, len(right)))
    assert result["error"] is None
    assert result["data"] == (expected,)


@pytest.mark.parametrize("materialized", ["left", "right", "neither"])
def test_compare_missing_pages_read_as_zero_without_allocation(materialized):
    runtimes = _runtimes(b": RUN COMPARE ;", external_size=16384)
    before = []
    for runtime in runtimes:
        region = _external(runtime)
        base = region.spec.base
        if materialized != "neither":
            runtime.memory.write8(base + (4095 if materialized == "left" else 12287), 0x80)
        before.append(set(region.pages))
    result = _compare(runtimes, "RUN", inputs=(base + 4093, 23, base + 12285, 23))
    assert result["error"] is None
    assert result["data"] == ((1,) if materialized == "left" else
                              (MASK64,) if materialized == "right" else (0,))
    assert [set(_external(r).pages) for r in runtimes] == before


@pytest.mark.parametrize("invalid_side", ["left", "right"])
def test_compare_validates_the_full_prefix_before_using_an_early_difference(invalid_side):
    runtimes = _runtimes(b": RUN COMPARE ;", external_size=4097)
    for runtime in runtimes:
        base = _external(runtime).spec.base
        runtime.memory.write_bytes(base, b"Axy")
        runtime.memory.write8(base + 4096, ord("Z"))
    a, b = (base + 4096, base) if invalid_side == "left" else (base, base + 4096)
    result = _compare(runtimes, "RUN", inputs=(a, 3, b, 3), require_native=False)
    assert result["error"] is not None
    assert result["data"] == ()
    assert result["counted_steps"] == 2


def test_compare_only_reads_the_common_prefix_and_ignores_empty_addresses():
    runtimes = _runtimes(b": RUN COMPARE ;", external_size=4097)
    for runtime in runtimes:
        base = _external(runtime).spec.base
        runtime.memory.write8(base, 7)
        runtime.memory.write8(base + 4096, 7)
    assert _compare(runtimes, "RUN", inputs=(base + 4096, MASK64, base, 1))["data"] == (1,)
    for runtime in runtimes:
        runtime.main_context.data.clear()
    assert _compare(runtimes, "RUN", inputs=(MASK64, 0, MMIO_BASE, MASK64))["data"] == (MASK64,)


@pytest.mark.parametrize("page_size", [1, 4, 16, 4096])
@pytest.mark.parametrize("value", [0, 0x100, 0x1AB])
@pytest.mark.parametrize("materialized", [False, True])
def test_fill_masks_bytes_and_preserves_sparse_page_allocation(page_size, value, materialized):
    runtimes = _runtimes(b": RUN FILL ;", external_size=8192, page_size=page_size)
    before = []
    for runtime in runtimes:
        region = _external(runtime)
        base = region.spec.base
        if materialized:
            runtime.memory.write_bytes(base + 4093, b"X" * 23)
        before.append(set(region.pages))
    result = _compare(runtimes, "RUN", inputs=(base + 4093, 23, value),
                      spans=((base + 4090, 29),),
                      require_native=materialized or value & 255 == 0)
    assert result["error"] is None
    assert result["memory"] == (bytes(3) + bytes([value & 255]) * 23 + bytes(3),)
    if value & 255 == 0:
        assert [set(_external(r).pages) for r in runtimes] == before
    assert set(_external(runtimes[0]).pages) == set(_external(runtimes[1]).pages)


@pytest.mark.parametrize("address_kind", ["region_end", "wrap", "mmio"])
def test_fill_declines_faulting_spans_before_any_partial_native_write(address_kind):
    runtimes = _runtimes(b": RUN FILL ;", external_size=4097)
    for runtime in runtimes:
        base = _external(runtime).spec.base
        runtime.memory.write8(base + 4096, 17)
    address = base + 4096 if address_kind == "region_end" else MASK64 if address_kind == "wrap" else MMIO_BASE
    result = _compare(runtimes, "RUN", inputs=(address, 4, 0xAA),
                      spans=((base + 4096, 1),), require_native=False)
    assert result["error"] is not None
    assert result["data"] == ()
    assert result["memory"] == (b"\x11",)


@pytest.mark.parametrize("operation,inputs", [
    (b"FILL", (MASK64, 0, 7)),
    (b"COMPARE", (MASK64, 0, MMIO_BASE, 0)),
])
@pytest.mark.parametrize("budget", [1, 2, 3])
def test_bulk_zero_length_operations_keep_both_ticks_without_address_access(operation, inputs, budget):
    result = _compare(_runtimes(b": RUN " + operation + b" ;"), "RUN",
                      inputs=inputs, step_budget=budget, require_native=budget > 1)
    assert result["counted_steps"] == budget


@pytest.mark.parametrize("operation,count", [(b"FILL", 3), (b"COMPARE", 4)])
def test_bulk_underflow_retains_reference_operand_consumption(operation, count):
    for available in range(count):
        result = _compare(_runtimes(b": RUN " + operation + b" ;"), "RUN",
                          inputs=tuple(range(available)), require_native=False)
        assert result["error"][0] is StackUnderflow
        assert result["data"] == ()
        assert result["counted_steps"] == 2


def test_fill_can_overwrite_live_and_popped_data_stack_cells():
    runtimes = _runtimes(b": RUN FILL ;")
    destination = runtimes[0].main_context.data.empty_pointer - 32
    result = _compare(runtimes, "RUN", inputs=(11, destination, 32, 0xAA))
    assert result["error"] is None
    assert result["data"] == (0xAAAAAAAAAAAAAAAA,)


def test_fill_of_return_stack_keeps_reference_cookie_and_capture_fault_handling():
    result = _compare(_runtimes(b": RUN RP@ 8 0 FILL ;"), "RUN")
    assert result["error"] is not None


def test_compare_and_fill_accept_more_than_one_full_page():
    runtimes = _runtimes(b": RUN >R 2DUP R> FILL 2DUP COMPARE ;", external_size=24576)
    # This test supplies ordinary storage rather than enlarging any step budget.
    for runtime in runtimes:
        base = _external(runtime).spec.base
        runtime.memory.write_bytes(base + 3, bytes(8207))
    result = _compare(runtimes, "RUN", inputs=(base + 3, 8207, 0x61), spans=((base, 8213),))
    assert result["error"] is None
    assert result["data"] == (0,)
    assert result["memory"] == (bytes(3) + b"a" * 8207 + bytes(3),)


def test_compare_nonempty_mmio_prefix_remains_a_reference_block_read_fault():
    runtimes = _runtimes(b": RUN COMPARE ;", external_size=4096)
    base = _external(runtimes[0]).spec.base
    result = _compare(runtimes, "RUN", inputs=(base, 1, MMIO_BASE, 1), require_native=False)
    assert result["error"] is not None
    assert result["data"] == ()


@pytest.mark.parametrize("operation,prefix", [(b"COMPARE", b"3 PICK"), (b"FILL", b"2 PICK")])
def test_bulk_hot_page_stays_clipped_at_a_partial_region_end(operation, prefix):
    runtimes = _runtimes(b": RUN " + prefix + b" C@ DROP " + operation + b" ;",
                        external_size=4097)
    for runtime in runtimes:
        base = _external(runtime).spec.base
        runtime.memory.write8(base + 4096, 11)
    inputs = (base + 4096, 3, base, 3) if operation == b"COMPARE" else (base + 4096, 3, 0xAA)
    result = _compare(runtimes, "RUN", inputs=inputs, spans=((base + 4096, 1),))
    assert result["error"] is not None
    assert result["memory"] == (b"\x0b",)


@pytest.mark.parametrize("operation", [b"COMPARE", b"FILL"])
def test_bulk_primitive_admission_keeps_original_binding_after_host_shadow(operation):
    runtimes = _runtimes(b": ORIGINAL " + operation + b" ;", external_size=4096)
    for runtime in runtimes:
        base = _external(runtime).spec.base
        runtime.memory.write_bytes(base, bytes(8))
        def shadow(context):
            context.data.clear()
            context.data.push(99)
        runtime.define_primitive(operation, shadow)
        runtime.evaluate(b": SHADOW " + operation + b" ;")
    inputs = (base, 8, base, 8) if operation == b"COMPARE" else (base, 8, 7)
    result = _compare(runtimes, "ORIGINAL", inputs=inputs, spans=((base, 8),))
    assert result["error"] is None
    assert result["data"] == ((0,) if operation == b"COMPARE" else ())
    for runtime in runtimes:
        runtime.main_context.data.clear()
    result = _compare(runtimes, "SHADOW", inputs=inputs, require_native=False)
    assert result["data"] == (99,)
