"""Bulk NTT transfer admits only the ordinary callback-free byte route."""

from contextlib import contextmanager
import random
import struct
import sys

import pytest

from shared.cells import MASK64
from shared.ntt import NTT_DEFAULT_MODULUS, NTT_DILITHIUM_MODULUS, NTT_SIZE
from simulator import memory as memory_module
from simulator.memory import EXTERNAL_BASE, MMIO_BASE, MemoryAccessError, SparseAddressSpace
from simulator.ntt import HostedNTTService
from simulator.runtime import MegaForthRuntime


SIZE = NTT_SIZE * 4


class ScalarService(HostedNTTService):
    """An unmodified subclass deliberately retains the original byte loop."""


def _service(kind=HostedNTTService):
    service = kind()
    service._poly_a[:] = [101] * NTT_SIZE
    service._poly_b[:] = [202] * NTT_SIZE
    service._result[:] = [0x1_2345_6789 + index * 0x10203 for index in range(NTT_SIZE)]
    service._load_a_stage[:] = b"old!"
    service._load_b_stage[:] = b"keep"
    service._index = 73
    service._busy = service._done = True
    return service


def _state(service):
    return (service.polynomial_a(), service.polynomial_b(), service.result(),
            service.load_stage(0), service.load_stage(1), service.index,
            service.modulus, service.roots, service.status)


def _payload(seed=2468):
    randomizer = random.Random(seed)
    values = [randomizer.randrange(1 << 32) for _ in range(NTT_SIZE)]
    return struct.pack("<256I", *values), values


@contextmanager
def _routes(memory, spans=()):
    """Observe calls without changing an identity used by bulk admission."""

    calls = {name: 0 for name in ("read8", "write8", "read_bytes", "write_bytes")}
    codes = {getattr(SparseAddressSpace, name).__code__: name for name in calls}
    previous = sys.getprofile()

    def observe(frame, event, arg):
        if event == "call" and frame.f_locals.get("self") is memory:
            name = codes.get(frame.f_code)
            address = frame.f_locals.get("address")
            if name is not None and (not spans or any(base <= address < base + size for base, size in spans)):
                calls[name] += 1
        if previous is not None:
            previous(frame, event, arg)

    sys.setprofile(observe)
    try:
        yield calls
    finally:
        sys.setprofile(previous)


@pytest.mark.parametrize("dense", (False, True))
@pytest.mark.parametrize("selector", (0, 7))
@pytest.mark.parametrize("modulus", (NTT_DEFAULT_MODULUS, NTT_DILITHIUM_MODULUS))
def test_ordinary_page_crossing_transfer_matches_scalar_oracle(dense, selector, modulus):
    memory = SparseAddressSpace(bank0_size=4096, page_size=64,
                                dense_backing=True if dense else None)
    reference = SparseAddressSpace(bank0_size=4096, page_size=64,
                                   dense_backing=True if dense else None)
    service, scalar = _service(), _service(ScalarService)
    payload, values = _payload()
    for owner in (memory, reference):
        owner.write_bytes(63, payload)
    for owner in (service, scalar):
        owner.set_modulus(modulus)
    with _routes(memory) as calls:
        service.load(63, selector, memory)
        service.store(2047, memory)
    scalar.load(63, selector, reference)
    scalar.store(2047, reference)
    assert _state(service) == _state(scalar)
    assert service.index == 0 and service.status == 3
    assert service.load_stage(selector) == payload[-4:]
    assert (service.polynomial_a() if selector == 0 else service.polynomial_b()) == tuple(
        value % modulus for value in values)
    assert memory.read_bytes(0, 4096) == reference.read_bytes(0, 4096)
    assert calls == {"read8": 0, "write8": 0, "read_bytes": 1, "write_bytes": 1}


def test_unwritten_sparse_source_is_zero_without_materializing_pages():
    memory = SparseAddressSpace(bank0_size=SIZE, page_size=64)
    service = _service()
    with _routes(memory) as calls:
        service.load(0, 0, memory)
    assert memory.resident_page_count == 0
    assert service.polynomial_a() == (0,) * NTT_SIZE
    assert service.polynomial_b() == (202,) * NTT_SIZE
    assert service.load_stage(0) == bytes(4)
    assert calls["read_bytes"] == 1 and calls["read8"] == 0


@pytest.mark.parametrize("modulus", (0, 17))
def test_unsupported_modulus_retains_byte_route_and_zero_failure_stage(modulus):
    memory = SparseAddressSpace(bank0_size=SIZE)
    payload, values = _payload()
    memory.write_bytes(0, payload)
    service = _service()
    service.set_modulus(modulus)
    before = _state(service)
    with _routes(memory) as calls:
        if modulus == 0:
            with pytest.raises(ZeroDivisionError):
                service.load(0, 0, memory)
        else:
            service.load(0, 0, memory)
    assert calls["read_bytes"] == 0
    assert calls["read8"] == (4 if modulus == 0 else SIZE)
    assert service.index == 0
    assert service.polynomial_b() == before[1]
    assert service.result() == before[2] and service.status == before[-1]
    assert service.load_stage(0) == (payload[:4] if modulus == 0 else payload[-4:])
    assert service.polynomial_a() == (before[0] if modulus == 0 else tuple(value % modulus for value in values))
    with _routes(memory) as calls:
        service.store(0, memory)
    assert calls["write8"] == SIZE and calls["write_bytes"] == 0


@pytest.mark.parametrize("prefix", (0, 1, 2, 3, 4, 5, 6, 7, SIZE - 1))
@pytest.mark.parametrize("operation", ("load", "store"))
@pytest.mark.parametrize("dense", (False, True))
def test_short_span_fault_keeps_exact_scalar_prefix_and_error(prefix, operation, dense):
    payload, _ = _payload()
    observed = []
    for kind in (HostedNTTService, ScalarService):
        memory = SparseAddressSpace(bank0_size=prefix, dense_backing=True if dense else None)
        if prefix:
            memory.write_bytes(0, payload[:prefix])
        service = _service(kind)
        with _routes(memory) as calls, pytest.raises(MemoryAccessError) as caught:
            if operation == "load":
                service.load(0, 0, memory)
            else:
                service.store(0, memory)
        error = caught.value
        assert (error.address, error.length, error.operation) == (prefix, 1, "read" if operation == "load" else "write")
        assert calls["read_bytes"] == calls["write_bytes"] == 0
        assert service.index == (prefix // 4 + int(operation == "store" and prefix % 4 == 3)) % NTT_SIZE
        observed.append((_state(service), memory.read_bytes(0, prefix), type(error), str(error)))
    assert observed[0] == observed[1]


def test_adjacent_regions_keep_byte_routing_even_when_every_byte_is_mapped():
    memory = SparseAddressSpace(bank0_size=EXTERNAL_BASE, external_size=SIZE)
    address = EXTERNAL_BASE - SIZE // 2
    payload, values = _payload()
    memory.write_bytes(address, payload[:SIZE // 2])
    memory.write_bytes(EXTERNAL_BASE, payload[SIZE // 2:])
    service = _service()
    with _routes(memory) as calls:
        service.load(address, 0, memory)
        service.store(address, memory)
    assert service.polynomial_a() == tuple(value % service.modulus for value in values)
    result = memory.read_bytes(address, SIZE // 2) + memory.read_bytes(EXTERNAL_BASE, SIZE // 2)
    assert result == struct.pack("<256I", *(value & 0xFFFF_FFFF for value in service.result()))
    assert calls == {"read8": SIZE, "write8": SIZE, "read_bytes": 0, "write_bytes": 0}


class Port:
    def __init__(self, fail_at=None):
        self.fail_at = fail_at
        self.reads, self.writes = [], []

    def preflight(self, offset, width, *, write):
        if offset == self.fail_at:
            raise ValueError("device transfer fault")

    def read8(self, offset):
        self.reads.append(offset)
        return offset & 255

    def write8(self, offset, value):
        self.writes.append((offset, value))


@pytest.mark.parametrize("operation", ("load", "store"))
@pytest.mark.parametrize("fail_at", (None, 7))
def test_mmio_callbacks_and_fault_prefix_remain_scalar(operation, fail_at):
    observations = []
    for kind in (HostedNTTService, ScalarService):
        port = Port(fail_at)
        memory = SparseAddressSpace(mmio=port)
        service = _service(kind)
        error = None
        try:
            if operation == "load":
                service.load(MMIO_BASE, 1, memory)
            else:
                service.store(MMIO_BASE, memory)
        except MemoryAccessError as caught:
            error = (type(caught), caught.address, caught.length, caught.operation,
                     type(caught.__cause__), str(caught.__cause__))
        assert bool(error) == (fail_at is not None)
        observations.append((_state(service), port.reads, port.writes, error))
    assert observations[0] == observations[1]
    assert len(observations[0][1 if operation == "load" else 2]) == (SIZE if fail_at is None else fail_at)


@pytest.mark.parametrize("address", (SIZE + 4, MASK64 - 2))
def test_unmapped_or_wrapping_span_reports_first_byte_fault_not_preflight(address):
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    for operation in ("load", "store"):
        with pytest.raises(MemoryAccessError) as caught:
            if operation == "load":
                service.load(address, 0, memory)
            else:
                service.store(address, memory)
        assert (caught.value.address, caught.value.length) == (address, 1)
        assert service.index == 0


def test_custom_wrapping_memory_observes_every_wrapped_byte():
    class WrappedMemory(SparseAddressSpace):
        def read8(self, address):
            self.reads.append(address)
            return address & 255

        def write8(self, address, value):
            self.writes.append((address, value))

    memory = WrappedMemory()
    memory.reads, memory.writes = [], []
    service = _service()
    address = MASK64 - 2
    service.load(address, 0, memory)
    service.store(address, memory)
    expected_addresses = [(address + offset) & MASK64 for offset in range(SIZE)]
    assert memory.reads == expected_addresses
    assert [address for address, _ in memory.writes] == expected_addresses
    assert service.load_stage(0) == bytes(value & 255 for value in expected_addresses[-4:])
    assert service.index == 0


@pytest.mark.parametrize("method", (
    "read8", "write8", "_read_integer", "_write_integer", "_region_at",
    "read_bytes", "write_bytes", "_resolve", "_qualify_ordinary_span",
))
def test_late_memory_override_declines_bulk_without_calling_custom_preflight(monkeypatch, method):
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    service.load(0, 0, memory)
    service.store(0, memory)
    original = getattr(memory, method)
    observed = []

    def custom(*args, **kwargs):
        observed.append(args)
        return original(*args, **kwargs)

    monkeypatch.setattr(memory, method, custom)
    with _routes(memory) as calls:
        service.load(0, 0, memory)
        service.store(0, memory)
    assert calls["read8"] == calls["write8"] == SIZE
    assert calls["read_bytes"] == calls["write_bytes"] == 0
    assert bool(observed) == (method in ("read8", "write8", "_read_integer", "_write_integer", "_region_at"))


@pytest.mark.parametrize("before_construction", (False, True))
def test_class_scalar_override_preserves_initial_and_late_byte_behavior(monkeypatch, before_construction):
    if not before_construction:
        memory = SparseAddressSpace(bank0_size=SIZE)
    original = SparseAddressSpace._read_integer

    def shifted(self, address, width):
        return (original(self, address, width) + 1) & 255

    monkeypatch.setattr(SparseAddressSpace, "_read_integer", shifted)
    if before_construction:
        memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    service.load(0, 0, memory)
    assert service.polynomial_a() == (0x01010101 % service.modulus,) * NTT_SIZE
    assert service.load_stage(0) == b"\x01" * 4


@pytest.mark.parametrize("operation,fail_at", (("load", 6), ("store", 7)))
def test_late_byte_fault_after_success_preserves_applied_prefix(monkeypatch, operation, fail_at):
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    service.load(0, 0, memory)
    service.store(0, memory)
    payload, values = _payload(1357)
    memory.write_bytes(0, payload)
    before = _state(service)
    method = "read8" if operation == "load" else "write8"
    original = getattr(memory, method)
    calls = []
    fault = RuntimeError("late byte failure")

    def fail(address, *args):
        calls.append(address)
        if address == fail_at:
            raise fault
        return original(address, *args)

    monkeypatch.setattr(memory, method, fail)
    with pytest.raises(RuntimeError) as caught:
        if operation == "load":
            service.load(0, 0, memory)
        else:
            service.store(0, memory)
    assert caught.value is fault
    assert calls == list(range(fail_at + 1))
    assert service.polynomial_b() == before[1]
    assert service.result() == before[2] and service.status == before[-1]
    if operation == "load":
        stage = bytearray(before[3])
        for offset in range(fail_at):
            stage[offset % 4] = payload[offset]
        assert service.load_stage(0) == bytes(stage)
        assert service.polynomial_a() == (values[0] % service.modulus,) + before[0][1:]
        assert service.index == 1
        assert memory.read_bytes(0, SIZE) == payload
    else:
        expected = struct.pack("<256I", *(value & 0xFFFF_FFFF for value in before[2]))
        assert memory.read_bytes(0, SIZE) == expected[:fail_at] + payload[fail_at:]
        assert service.index == 2
        assert service.load_stage(0) == before[3]


@pytest.mark.parametrize("kind", ("sparse_method", "sparse_page", "sparse_dictionary", "dense_method"))
def test_custom_backing_retains_scalar_access(monkeypatch, kind):
    dense = kind == "dense_method"
    memory = SparseAddressSpace(bank0_size=SIZE, dense_backing=True if dense else None)
    memory.write_bytes(0, bytes(SIZE))
    region = memory._regions[0]
    seen = []
    if kind.endswith("method"):
        original = type(region).read_integer

        def read(owner, offset, width):
            seen.append(offset)
            return original(owner, offset, width) + 1

        monkeypatch.setattr(type(region), "read_integer", read)
    elif kind == "sparse_page":
        class Page(bytearray):
            def __getitem__(self, index):
                seen.append(index)
                return super().__getitem__(index)
        region.pages[0] = Page(region.pages[0])
    else:
        class Pages(dict):
            def get(self, key, default=None):
                seen.append(key)
                return super().get(key, default)
        region.pages = Pages(region.pages)
    service = _service()
    with _routes(memory) as calls:
        service.load(0, 0, memory)
    assert calls["read8"] == SIZE and calls["read_bytes"] == 0
    assert len(seen) == (0 if kind == "sparse_page" else SIZE)
    expected = 0x01010101 % service.modulus if kind.endswith("method") else 0
    assert service.polynomial_a() == (expected,) * NTT_SIZE


def test_custom_result_container_observes_each_device_read_before_guest_write():
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    reads = []

    class Results(list):
        def __getitem__(self, index):
            reads.append((index, service.index))
            return super().__getitem__(index)

    service._result = Results(service._result)
    with _routes(memory) as calls:
        service.store(0, memory)
    assert reads == [(index, index) for index in range(NTT_SIZE)]
    assert calls["write8"] == SIZE and calls["write_bytes"] == 0


@pytest.mark.parametrize("method", ("_cell", "_memory"))
def test_late_service_helper_override_keeps_original_byte_calls(monkeypatch, method):
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    service.load(0, 0, memory)
    original = getattr(service, method)
    seen = []

    def observe(*args, **kwargs):
        seen.append(args)
        return original(*args, **kwargs)

    monkeypatch.setattr(service, method, observe)
    with _routes(memory) as calls:
        service.load(0, 0, memory)
        service.store(0, memory)
    assert seen
    assert calls["read8"] == calls["write8"] == SIZE
    assert calls["read_bytes"] == calls["write_bytes"] == 0


@pytest.mark.parametrize("helper", ("_checked_span", "_require_integer"))
def test_changed_memory_global_helper_preserves_scalar_callbacks(monkeypatch, helper):
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    original = getattr(memory_module, helper)
    seen = []

    def observe(*args, **kwargs):
        seen.append(args)
        return original(*args, **kwargs)

    monkeypatch.setattr(memory_module, helper, observe)
    with _routes(memory) as calls:
        service.load(0, 0, memory)
        service.store(0, memory)
    assert seen
    assert calls["read8"] == calls["write8"] == SIZE
    assert calls["read_bytes"] == calls["write_bytes"] == 0


@pytest.mark.parametrize("owner_name,collision", (("memory", "read_bytes"), ("service", "store")))
@pytest.mark.parametrize("key_kind", ("object", "str_subclass"))
def test_noncanonical_namespace_keys_cannot_run_callbacks_during_admission(owner_name, collision, key_kind):
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()
    seen = []

    class Key:
        def __hash__(self):
            seen.append("hash")
            return hash(collision)

        def __eq__(self, other):
            seen.append(("equality", other))
            # The old qualification could reach this after checking read8.
            memory.read8 = lambda address: 255
            return False

    class TextKey(str):
        __hash__ = Key.__hash__
        __eq__ = Key.__eq__

    key = Key() if key_kind == "object" else TextKey(collision)
    owner = memory if owner_name == "memory" else service
    owner.__dict__[key] = None
    seen.clear()
    with _routes(memory) as calls:
        service.load(0, 0, memory)
    assert seen == []
    assert calls["read8"] == SIZE and calls["read_bytes"] == 0
    assert service.polynomial_a() == (0,) * NTT_SIZE
    assert service.load_stage(0) == bytes(4)


def test_scalar_helper_identity_is_not_proved_by_user_equality(monkeypatch):
    memory = SparseAddressSpace(bank0_size=SIZE)
    memory.write_bytes(0, bytes(SIZE))
    service = _service()
    original = memory_module.struct.unpack_from
    seen = {"equality": 0, "calls": 0}

    class Unpack:
        def __eq__(self, other):
            seen["equality"] += 1
            return True

        def __call__(self, *args):
            seen["calls"] += 1
            return (original(*args)[0] ^ 1,)

    monkeypatch.setattr(memory_module.struct, "unpack_from", Unpack())
    with _routes(memory) as calls:
        service.load(0, 0, memory)
    assert seen == {"equality": 0, "calls": SIZE}
    assert calls["read8"] == SIZE and calls["read_bytes"] == 0
    assert service.polynomial_a() == (0x01010101 % service.modulus,) * NTT_SIZE


def test_rebound_memory_class_alias_declines_bulk(monkeypatch):
    memory = SparseAddressSpace(bank0_size=SIZE)
    service = _service()

    class CustomMemory(SparseAddressSpace):
        pass

    monkeypatch.setattr(memory_module, "SparseAddressSpace", CustomMemory)
    with _routes(memory) as calls:
        service.load(0, 0, memory)
        service.store(0, memory)
    assert calls["read8"] == calls["write8"] == SIZE
    assert calls["read_bytes"] == calls["write_bytes"] == 0


@pytest.mark.parametrize("execution_backend", ("python", "native"))
def test_both_semantic_executors_use_qualified_ntt_transfer(execution_backend):
    if execution_backend == "native":
        pytest.importorskip("_megaforth_native")
    runtime = MegaForthRuntime(execution_backend=execution_backend)
    source, destination = 0x60000, 0x61000
    runtime.memory.write_bytes(source, b"\x01" + bytes(SIZE - 1))
    with _routes(runtime.memory, ((source, SIZE), (destination, SIZE))) as calls:
        runtime.main_context.data.push(source)
        runtime.main_context.data.push(0)
        runtime.execute("NTT-LOAD")
        runtime.execute("NTT-FWD")
        runtime.main_context.data.push(destination)
        runtime.execute("NTT-STORE")
    assert runtime.memory.read_bytes(destination, SIZE) == b"\x01\x00\x00\x00" * NTT_SIZE
    assert runtime.ntt.index == 0
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert calls == {"read8": 0, "write8": 0, "read_bytes": 1, "write_bytes": 1}
