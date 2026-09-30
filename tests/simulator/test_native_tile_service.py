"""Hosted effects remain Python-owned around the shared native tile values."""

from __future__ import annotations

import importlib

import pytest

from shared import ieee_fp as fp, tile_float as tf, tile_formats as formats
from shared.cells import MASK64
from simulator.field import HostedFieldALUService
from simulator.memory import MemoryAccessError, SparseAddressSpace
from simulator.runtime import MegaForthRuntime
from simulator.tile import HostedTileService, UnsupportedTileModeError


SOURCE0, SOURCE1, DESTINATION = 0x100, 0x300, 0x500
SIZE = 0x1000
ELEMENTWISE = (
    "add", "subtract", "bitwise_and", "bitwise_or", "bitwise_xor",
    "elementwise_minimum", "elementwise_maximum", "absolute", "multiply",
)
REDUCTIONS = (
    "dot", "sum", "sum_squares", "l1_norm", "minimum", "maximum",
    "minimum_index", "maximum_index",
)


@pytest.fixture
def native_values():
    extension = importlib.import_module("_megaforth_native")
    assert extension.TILE_VALUES_API_VERSION == 1
    return extension.tile_execute_values


def _tile(ew, values, *, size=64):
    lane = formats.decode(ew)
    raw = [fp.from_double(lane.float_format, value) if lane.is_float else value
           for value in values]
    return bytes(tf.pack_bits(lane.lane_bits, [
        raw[index % len(raw)] & lane.lane_mask
        for index in range(size // lane.lane_bytes)
    ]))


def _make(execute=None, *, mode=7, control=3, memory=None,
          registers=None, service_type=HostedTileService, account=None):
    memory = memory if memory is not None else SparseAddressSpace(bank0_size=SIZE)
    registers = registers if registers is not None else HostedFieldALUService(core_count=1)
    calls, accounts = [], []
    service = service_type(
        memory, registers,
        account_operation=account if account is not None else lambda: accounts.append(1),
    )
    service.set_mode(mode)
    service.set_control(control)
    service.set_source0(SOURCE0)
    service.set_source1(SOURCE1)
    service.set_destination(DESTINATION)
    registers.replace_accumulator_words(0, (11, 22, 33, 44))
    if execute is not None:
        def tracked(*args, **kwargs):
            calls.append((args, kwargs))
            return execute(*args, **kwargs)
        assert service.bind_native_values(tracked)
    return service, memory, registers, calls, accounts


def _fill(memory, ew):
    memory.write_bytes(SOURCE0, _tile(ew, (-3, 0, 1, 2, 5, -1, 7, 4), size=512))
    memory.write_bytes(SOURCE1, _tile(ew, (2, -1, 0, 3, 1, 7, 2, 1), size=512))
    memory.write_bytes(DESTINATION, _tile(ew, (1, -1, 0, 3), size=512))


def _snapshot(bundle):
    service, memory, _registers, _calls, accounts = bundle
    return (memory.read_bytes(0, SIZE), service.accumulator, service.control,
            service.mode, service.source0, service.source1, service.destination,
            tuple(accounts))


@pytest.mark.parametrize("ew", range(8))
@pytest.mark.parametrize("control", range(4))
def test_native_candidates_preserve_service_state_and_all_accumulator_controls(native_values, ew, control):
    operations = [(name, ()) for name in (*ELEMENTWISE, *REDUCTIONS,
                  "multiply_accumulate", "fused_multiply_add", "popcount", "transpose", "select")]
    if ew != 7:
        operations.append(("widening_multiply", ()))
    if ew >= 4:
        operations.extend((("divide", ()), ("square_root", ())))
    operations.extend(("compare_mask", (predicate,)) for predicate in range(8))
    operations.extend(("convert", (target,)) for target in range(8)
                      if target != ew and (target >= 4 or ew >= 4))
    mode = ew | (0x70 if ew >= 4 else 0x30)
    for operation, arguments in operations:
        reference = _make(mode=mode, control=control)
        accelerated = _make(native_values, mode=mode, control=control)
        for bundle in (reference, accelerated):
            _fill(bundle[1], ew)
            getattr(bundle[0], operation)(*arguments)
        assert _snapshot(accelerated) == _snapshot(reference), (ew, control, operation, arguments)
        portable = (operation in ("popcount", "transpose") or
                    ew < 4 and operation in (*REDUCTIONS, "multiply_accumulate",
                                             "fused_multiply_add", "widening_multiply"))
        assert len(accelerated[3]) == (0 if portable else 1), (ew, operation)
        assert accelerated[4] == [1]


@pytest.mark.parametrize("phase", ("before_bind", "after_bind"))
def test_custom_lane_helper_is_honored_before_and_after_binding(native_values, monkeypatch, phase):
    service, memory, _registers, _calls, accounts = _make(mode=4)
    _fill(memory, 4)
    calls, observed = [], []

    def tracked(*args, **kwargs):
        calls.append(args[0])
        return native_values(*args, **kwargs)

    if phase == "after_bind":
        assert service.bind_native_values(tracked)

    def customized(lane_format, operation, left, right):
        observed.append((lane_format, operation, tuple(left), tuple(right)))
        return [fp.from_double(lane_format, 42.0)] * len(left)

    monkeypatch.setattr(tf, "elementwise", customized)
    if phase == "before_bind":
        assert not service.bind_native_values(tracked)
    service.add()
    assert memory.read_bytes(DESTINATION, 64) == _tile(4, (42,))
    assert len(observed) == 1
    assert calls == [] and accounts == [1]


@pytest.mark.parametrize("operation", ("sum", "sum_squares", "dot"))
@pytest.mark.parametrize("signed", (False, True))
def test_unextracted_integer_reductions_keep_the_full_256_bit_accumulator(native_values, operation, signed):
    bundle = _make(native_values, mode=3 | (0x10 if signed else 0), control=1)
    service, memory, registers, calls, accounts = bundle
    memory.write_bytes(SOURCE0, MASK64.to_bytes(8, "little") * 8)
    memory.write_bytes(SOURCE1, MASK64.to_bytes(8, "little") * 8)
    registers.replace_accumulator_words(0, (MASK64, MASK64, 0, 0))
    value = -1 if signed else MASK64
    addition = 8 * (value if operation == "sum" else value * value)
    expected = (((1 << 128) - 1) + addition) % (1 << 256)
    getattr(service, operation)()
    assert service.accumulator == tuple((expected >> (64 * index)) & MASK64 for index in range(4))
    assert service.control == 1
    assert calls == [] and accounts == [1]


@pytest.mark.parametrize("ew", (4, 5))
def test_index_accumulation_preserves_old_payload_when_candidate_does_not_replace(native_values, ew):
    service, memory, registers, calls, _accounts = _make(native_values, mode=ew, control=1)
    memory.write_bytes(SOURCE0, _tile(ew, (5, 6, 7)))
    old = 0xAABBCCDD00000000 | fp.from_double(fp.FP32, -9.0)
    registers.replace_accumulator_words(0, (0x123456789ABCDEF0, old, 33, 44))
    service.minimum_index()
    assert service.accumulator == (0x123456789ABCDEF0, old, 0, 0)
    assert len(calls) == 1 and service.control == 1


@pytest.mark.parametrize("operation,arguments", (
    ("add", ()), ("fused_multiply_add", ()), ("select", ()),
    ("widening_multiply", ()), ("convert", (0,)), ("convert", (7,)),
))
@pytest.mark.parametrize("offset", (0, 1, 32, 64))
def test_sources_are_snapshotted_before_aliasing_destination_writes(native_values, operation, arguments, offset):
    mode = 4
    reference, accelerated = _make(mode=mode), _make(native_values, mode=mode)
    for bundle in (reference, accelerated):
        _fill(bundle[1], mode)
        bundle[0].set_destination(SOURCE0 + offset)
        getattr(bundle[0], operation)(*arguments)
    assert _snapshot(accelerated) == _snapshot(reference)
    assert len(accelerated[3]) == 1


@pytest.mark.parametrize("operation,arguments", (
    ("widening_multiply", ()), ("convert", (7,)),
))
def test_late_output_fault_retains_completed_tile_prefix_without_accounting(native_values, operation, arguments):
    ew = 4 if operation == "widening_multiply" else 0
    service, memory, _registers, calls, accounts = _make(native_values, mode=ew)
    memory.write_bytes(SOURCE0, _tile(ew, (2,)))
    memory.write_bytes(SOURCE1, _tile(ew, (3,)))
    service.set_destination(SIZE - 64)
    memory.write_bytes(SIZE - 64, b"\xA5" * 64)
    with pytest.raises(MemoryAccessError):
        getattr(service, operation)(*arguments)
    expected = _tile(6, (6,)) if operation == "widening_multiply" else _tile(7, (2,))
    assert memory.read_bytes(SIZE - 64, 64) == expected
    assert service.accumulator == (11, 22, 33, 44) and service.control == 3
    assert len(calls) == 1 and accounts == []


@pytest.mark.parametrize("operation,arguments,failed_operand", (
    ("absolute", (), "source1"), ("add", (), "source0"),
    ("fused_multiply_add", (), "destination"), ("select", (), "destination"),
    ("minimum_index", (), "source0"), ("convert", (0,), "source0"),
))
def test_required_input_fault_precedes_native_computation_and_all_effects(native_values, operation, arguments, failed_operand):
    bundle = _make(native_values, mode=7)
    service, memory, _registers, calls, accounts = bundle
    _fill(memory, 7)
    # Narrowing conversion needs eight complete input tiles; only its first
    # tile exists. Other operations fail their single required read.
    fault_address = SIZE - (64 if operation == "convert" else 32)
    getattr(service, "set_" + failed_operand)(fault_address)
    before = _snapshot(bundle)
    with pytest.raises(MemoryAccessError):
        getattr(service, operation)(*arguments)
    assert _snapshot(bundle) == before
    assert calls == [] and accounts == []


def test_square_root_does_not_read_unused_source1(native_values):
    service, memory, _registers, calls, accounts = _make(native_values)
    memory.write_bytes(SOURCE0, _tile(7, (4,)))
    service.set_source1(SIZE)
    service.square_root()
    assert memory.read_bytes(DESTINATION, 64) == _tile(7, (2,))
    assert len(calls) == 1 and accounts == [1]


@pytest.mark.parametrize("mode,operation,arguments", (
    (8, "add", ()), (0, "divide", ()), (3, "square_root", ()),
    (7, "widening_multiply", ()), (7, "compare_mask", (8,)),
    (0, "convert", (1,)), (7, "convert", (7,)), (7, "convert", (15,)),
))
def test_rejected_admission_precedes_invalid_memory_or_native_calls(native_values, mode, operation, arguments):
    bundle = _make(native_values, mode=mode)
    service = bundle[0]
    service.set_source0(SIZE)
    service.set_source1(SIZE)
    service.set_destination(SIZE)
    before = _snapshot(bundle)
    with pytest.raises(UnsupportedTileModeError):
        getattr(service, operation)(*arguments)
    assert _snapshot(bundle) == before
    assert bundle[3] == []


@pytest.mark.parametrize("phase", ("before_bind", "after_bind"))
@pytest.mark.parametrize("owner,name", (
    ("memory", "read_bytes"), ("memory", "write_bytes"),
    ("memory", "_resolve"), ("memory", "_region_at"),
    ("memory", "_read_integer"), ("memory", "_write_integer"),
    ("registers", "operand_address"), ("registers", "result_address"),
    ("registers", "accumulator_words"), ("registers", "replace_accumulator_words"),
    ("registers", "_state"), ("registers", "_cell"),
))
def test_custom_memory_and_register_helpers_keep_reference_values(native_values, monkeypatch, phase, owner, name):
    service, memory, registers, _calls, accounts = _make(mode=7, control=1)
    _fill(memory, 7)
    calls = []

    def tracked(*args, **kwargs):
        calls.append(args[0])
        return native_values(*args, **kwargs)

    if phase == "after_bind":
        assert service.bind_native_values(tracked)
    target = memory if owner == "memory" else registers
    original = getattr(target, name)
    observed = []

    def customized(*args, **kwargs):
        observed.append((args, kwargs))
        return original(*args, **kwargs)

    monkeypatch.setattr(target, name, customized)
    if phase == "before_bind":
        assert not service.bind_native_values(tracked)
    service.add()
    service.sum()
    assert calls == [] and accounts == [1, 1]
    if name not in ("_read_integer", "_write_integer"):
        assert observed


@pytest.mark.parametrize("owner", ("memory", "registers", "service"))
def test_subclass_services_and_views_are_not_selected(native_values, owner):
    class Memory(SparseAddressSpace):
        pass

    class Registers(HostedFieldALUService):
        pass

    class Service(HostedTileService):
        pass

    bundle = _make(
        memory=Memory(bank0_size=SIZE) if owner == "memory" else None,
        registers=Registers(core_count=1) if owner == "registers" else None,
        service_type=Service if owner == "service" else HostedTileService,
    )
    assert not bundle[0].bind_native_values(native_values)
    _fill(bundle[1], 7)
    bundle[0].add()
    assert bundle[4] == [1]


def test_late_memory_callback_keeps_mode_capture_and_read_order(native_values, monkeypatch):
    service, memory, _registers, calls, accounts = _make(native_values, mode=0)
    memory.write_bytes(SOURCE0, bytes((0xFF,)) * 64)
    memory.write_bytes(SOURCE1, bytes((2,)) * 64)
    original = memory.read_bytes
    events = []

    def read(address, length):
        events.append(address)
        service.set_mode(0x20)  # Saturation changes after _binary captured it.
        return original(address, length)

    monkeypatch.setattr(memory, "read_bytes", read)
    service.add()
    assert original(DESTINATION, 64) == bytes((1,)) * 64
    assert events == [SOURCE0, SOURCE1]
    assert service.mode == 0x20 and calls == [] and accounts == [1]


@pytest.mark.parametrize("mode,operation,expected_control", ((0, "sum", 3), (7, "sum", 1), (0, "minimum", 1)))
def test_publication_failure_retains_existing_zero_control_order(native_values, monkeypatch, mode, operation, expected_control):
    service, memory, registers, calls, accounts = _make(native_values, mode=mode)
    memory.write_bytes(SOURCE0, _tile(mode, (2,)))

    def rejected(*_args):
        raise RuntimeError("publication stopped")

    monkeypatch.setattr(registers, "replace_accumulator_words", rejected)
    with pytest.raises(RuntimeError, match="publication stopped"):
        getattr(service, operation)()
    assert service.control == expected_control
    assert service.accumulator == (11, 22, 33, 44)
    assert calls == [] and accounts == []


@pytest.mark.parametrize("operation", ("add", "sum"))
def test_accountant_observes_native_publication_and_failure_does_not_undo_it(native_values, operation):
    observed = []

    def account():
        observed.append((memory.read_bytes(DESTINATION, 64), service.accumulator, service.control))
        raise RuntimeError("accountant stopped")

    service, memory, _registers, calls, _accounts = _make(native_values, account=account)
    memory.write_bytes(SOURCE0, _tile(7, (2,)))
    memory.write_bytes(SOURCE1, _tile(7, (3,)))
    with pytest.raises(RuntimeError, match="accountant stopped"):
        getattr(service, operation)()
    assert len(calls) == len(observed) == 1
    if operation == "add":
        assert observed == [(_tile(7, (5,)), (11, 22, 33, 44), 3)]
    else:
        assert observed == [(bytes(64), (fp.from_double(fp.FP64, 16.0), 0, 0, 0), 1)]


@pytest.mark.parametrize("backend", ("python", "native"))
def test_compiled_source_selects_only_native_backend_values_and_preserves_scalar_fpcsr(backend):
    runtime = MegaForthRuntime(execution_backend=backend)
    runtime.memory.write_bytes(SOURCE0, _tile(7, (2,)))
    runtime.memory.write_bytes(SOURCE1, _tile(7, (3,)))
    runtime.tile.set_mode(7)
    runtime.tile.set_source0(SOURCE0)
    runtime.tile.set_source1(SOURCE1)
    runtime.tile.set_destination(DESTINATION)
    runtime.scalar_float.write_fpcsr(7)
    runtime.evaluate(b": TILE-BOUNDARY 3 0 DO TADD LOOP ;")
    calls = []
    selected = runtime.tile._native_values
    if backend == "native":
        assert callable(selected)

        def tracked(*args, **kwargs):
            calls.append(args[0])
            return selected(*args, **kwargs)

        assert runtime.tile.bind_native_values(tracked)
    else:
        assert selected is None
    runtime.execute("TILE-BOUNDARY", step_budget=1000)
    assert runtime.memory.read_bytes(DESTINATION, 64) == _tile(7, (5,))
    assert runtime.scalar_float.fpcsr == 7
    assert runtime.diagnostics.perf_tileops == 3
    assert calls == (["add"] * 3 if backend == "native" else [])
