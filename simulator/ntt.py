"""Hosted semantic state for the shared 256-coefficient NTT engine.

The service models terminal polynomial values, register persistence, command
status, and the BIOS byte-transfer order.  It does not emulate the physical
MMIO aperture, command latency, bus arbitration, or hardware timing.
"""

from __future__ import annotations

import struct

from shared.cells import u64
from shared.ntt import (
    NTT_COEFFICIENT_BYTES,
    NTT_DEFAULT_MODULUS,
    NTT_SIZE,
    NTTRoots,
    find_ntt_roots,
    ntt_forward,
    ntt_inverse,
    ntt_pointwise_add,
    ntt_pointwise_multiply,
)
from simulator import memory as _memory_module
from simulator.memory import SparseAddressSpace


NTT_STATUS_IDLE = 0
NTT_STATUS_BUSY = 1
NTT_STATUS_DONE = 2


_TRANSFER_BYTES = NTT_SIZE * NTT_COEFFICIENT_BYTES
_TRANSFER_FORMAT = struct.Struct("<256I")
_MEMORY_TYPE = SparseAddressSpace
_MAX_NAMESPACE_KEYS = 128
_MEMORY_METHODS = tuple(
    (name, SparseAddressSpace.__dict__[name]) for name in (
        "read8", "write8", "_read_integer", "_write_integer", "read_bytes",
        "write_bytes", "_qualify_ordinary_span", "_resolve", "_region_at",
    )
)
_MEMORY_GLOBALS = tuple(
    (name, getattr(_memory_module, name)) for name in (
        "_checked_span", "_require_integer", "_ResolvedSpan",
        "_QualifiedOrdinarySpan", "_SparseRegion", "_DenseRegion", "struct",
        "AddressClass", "RegionSpec", "SparseAddressSpace", "MASK64", "MemoryAccessError",
        "ADDRESS_SPACE_SIZE", "MMIO_BASE", "MMIO_LIMIT", "_INTEGER_WIDTHS",
    )
)
_REGION_METHODS = tuple(
    (kind, tuple((name, kind.__dict__[name]) for name in
                 ("read", "write", "read_integer", "write_integer", *kind.__slots__)))
    for kind in (_memory_module._SparseRegion, _memory_module._DenseRegion)
)
_SPAN_INITIALIZERS = tuple(
    (kind, kind.__init__) for kind in
    (_memory_module._ResolvedSpan, _memory_module._QualifiedOrdinarySpan)
)
_SPAN_ATTRIBUTES = tuple(
    (kind, tuple((name, kind.__dict__[name]) for name in kind.__slots__))
    for kind in (_memory_module.RegionSpec, _memory_module._ResolvedSpan,
                 _memory_module._QualifiedOrdinarySpan)
)
_SPEC_LIMIT = _memory_module.RegionSpec.limit
_INTEGER_FORMATS = dict(_memory_module._INTEGER_FORMATS)
_STRUCT_SCALARS = (struct.unpack_from, struct.pack_into)
_CELL_MASK = u64.__globals__["MASK64"]


def _plain_namespace(attributes) -> bool:
    # A non-string or str-subclass key can run equality during a later name
    # lookup and change a route whose identity has already been checked.
    return (len(attributes) <= _MAX_NAMESPACE_KEYS
            and all(type(name) is str for name in attributes))


def _methods_match(owner: object, originals: tuple) -> bool:
    kind = type(owner)
    namespace = kind.__dict__
    if (not _plain_namespace(namespace)
            or kind.__getattribute__ is not object.__getattribute__
            or kind.__setattr__ is not object.__setattr__):
        return False
    try:
        attributes = object.__getattribute__(owner, "__dict__")
    except AttributeError:
        attributes = {}
    if type(attributes) is not dict or not _plain_namespace(attributes):
        return False
    return all(name not in attributes and namespace.get(name) is original
               for name, original in originals)


def _ordinary_transfer(service, memory, address: int) -> bool:
    """Prove a callback-free transfer, otherwise retain the original byte path.

    Declining qualification never publishes a whole-span error. A byte fault
    must still expose the original coefficient/staging/index prefix.
    """

    if (HostedNTTService is not _SERVICE_TYPE or SparseAddressSpace is not _MEMORY_TYPE
            or type(service) is not _SERVICE_TYPE
            or type(memory) is not _MEMORY_TYPE
            or type(address) is not int
            or not _methods_match(service, _SERVICE_METHODS)
            or not _methods_match(memory, _MEMORY_METHODS)
            or any(not _plain_namespace(kind.__dict__) for kind, _ in _SPAN_ATTRIBUTES)):
        return False
    if (any(getattr(_memory_module, name) is not original
            for name, original in _MEMORY_GLOBALS)
            or any(globals().get(name) is not original
                   for name, original in _SERVICE_GLOBALS)
            or u64.__globals__["MASK64"] is not _CELL_MASK
            or any(kind.__init__ is not original for kind, original in _SPAN_INITIALIZERS)
            or any(kind.__getattribute__ is not object.__getattribute__
                   for kind, _ in _SPAN_INITIALIZERS)
            or any(kind.__dict__.get(name) is not original
                   for kind, attributes in _SPAN_ATTRIBUTES for name, original in attributes)
            or _memory_module.RegionSpec.limit is not _SPEC_LIMIT
            or type(_memory_module._INTEGER_FORMATS) is not dict
            or any(type(key) is not int or type(value) is not str
                   for key, value in _memory_module._INTEGER_FORMATS.items())
            or _memory_module._INTEGER_FORMATS != _INTEGER_FORMATS
            or struct.unpack_from is not _STRUCT_SCALARS[0]
            or struct.pack_into is not _STRUCT_SCALARS[1]):
        return False
    if ("_regions" in SparseAddressSpace.__dict__
            or any(name in HostedNTTService.__dict__ for name in (
                "_modulus", "_roots", "_index", "_poly_a", "_poly_b", "_result",
                "_load_a_stage", "_load_b_stage"))):
        return False
    if (type(service._modulus) is not int or service._modulus <= 0
            or service._roots is None or type(memory._regions) is not tuple):
        return False
    buffers = (service._poly_a, service._poly_b, service._result)
    stages = (service._load_a_stage, service._load_b_stage)
    if (any(type(values) is not list or len(values) != NTT_SIZE
            or any(type(value) is not int for value in values) for values in buffers)
            or len({id(values) for values in buffers}) != 3
            or any(type(stage) is not bytearray or len(stage) != NTT_COEFFICIENT_BYTES
                   for stage in stages)
            or stages[0] is stages[1]):
        return False
    previous_limit = 0
    for region in memory._regions:
        methods = next((items for kind, items in _REGION_METHODS
                        if type(region) is kind), None)
        if (methods is None or not _methods_match(region, methods)
                or type(region.spec) is not _memory_module.RegionSpec
                or type(region.spec).__getattribute__ is not object.__getattribute__
                or type(region.spec.base) is not int or type(region.spec.size) is not int):
            return False
        spec = region.spec
        if (spec.base < previous_limit or spec.size <= 0
                or spec.base + spec.size > _memory_module.ADDRESS_SPACE_SIZE
                or spec.base < _memory_module.MMIO_LIMIT
                and _memory_module.MMIO_BASE < spec.limit):
            return False
        previous_limit = spec.limit
    try:
        span = memory._qualify_ordinary_span(address, _TRANSFER_BYTES)
    except (ValueError, _memory_module.MemoryAccessError):
        return False
    region = span._region
    if type(region) is _memory_module._SparseRegion:
        if (type(region.page_size) is not int or region.page_size <= 0
                or type(region.pages) is not dict
                or any(type(key) is not int for key in region.pages)):
            return False
        first = span._offset // region.page_size
        last = (span._offset + _TRANSFER_BYTES - 1) // region.page_size
        for index in range(first, last + 1):
            page = region.pages.get(index)
            if page is not None and (type(page) is not bytearray
                    or len(page) != region.page_size
                    or any(page is stage for stage in stages)):
                return False
    else:
        view = region._buffer
        if type(view) is not memoryview:
            return False
        try:
            if (view.readonly or not view.c_contiguous or view.format != "B"
                    or view.ndim != 1 or view.nbytes != region.spec.size
                    or type(view.obj) is not bytearray
                    or any(view.obj is stage for stage in stages)):
                return False
        except ValueError:  # A released view retains its original scalar fault.
            return False
    return True


class HostedNTTService:
    """One runtime-local NTT device shared by all semantic callers."""

    def __init__(self) -> None:
        self._modulus = NTT_DEFAULT_MODULUS
        self._index = 0
        self._poly_a = [0] * NTT_SIZE
        self._poly_b = [0] * NTT_SIZE
        self._result = [0] * NTT_SIZE
        self._busy = False
        self._done = False
        self._load_a_stage = bytearray(NTT_COEFFICIENT_BYTES)
        self._load_b_stage = bytearray(NTT_COEFFICIENT_BYTES)
        self._roots = find_ntt_roots(self._modulus)

    @property
    def modulus(self) -> int:
        """Return the retained uint64 modulus."""

        return self._modulus

    @property
    def index(self) -> int:
        """Return the retained coefficient index register."""

        return self._index

    @property
    def status(self) -> int:
        """Return the raw idle/busy/done status bits."""

        return (int(self._done) << 1) | int(self._busy)

    @property
    def roots(self) -> NTTRoots | None:
        """Return the current device-selected root tuple, if any."""

        return self._roots

    def polynomial_a(self) -> tuple[int, ...]:
        """Return a diagnostic snapshot of input buffer A."""

        return tuple(self._poly_a)

    def polynomial_b(self) -> tuple[int, ...]:
        """Return a diagnostic snapshot of input buffer B."""

        return tuple(self._poly_b)

    def result(self) -> tuple[int, ...]:
        """Return a diagnostic snapshot of the result buffer."""

        return tuple(self._result)

    def load_stage(self, selector: int) -> bytes:
        """Return the selected partial four-byte input staging register."""

        selector = self._cell(selector, label="NTT buffer selector")
        stage = self._load_a_stage if selector == 0 else self._load_b_stage
        return bytes(stage)

    def set_modulus(self, value: int) -> None:
        """Replace Q and recompute roots without changing buffers or status."""

        self._modulus = self._cell(value, label="NTT modulus")
        self._roots = find_ntt_roots(self._modulus)

    def set_index(self, value: int) -> None:
        """Replace the raw 16-bit coefficient index register."""

        self._index = self._cell(value, label="NTT index") & 0xFFFF

    def load(
        self,
        address: int,
        selector: int,
        memory: SparseAddressSpace,
    ) -> None:
        """Load 256 uint32 coefficients with BIOS byte/fault ordering."""

        address = self._cell(address, label="NTT source address")
        selector = self._cell(selector, label="NTT buffer selector")
        memory = self._memory(memory)
        stage = self._load_a_stage if selector == 0 else self._load_b_stage
        polynomial = self._poly_a if selector == 0 else self._poly_b
        self._index = 0
        if _ordinary_transfer(self, memory, address):
            payload = memory.read_bytes(address, _TRANSFER_BYTES)
            polynomial[:] = [value % self._modulus
                             for value in _TRANSFER_FORMAT.unpack(payload)]
            stage[:] = payload[-NTT_COEFFICIENT_BYTES:]
            return
        for coefficient in range(NTT_SIZE):
            source = u64(address + coefficient * NTT_COEFFICIENT_BYTES)
            for byte_index in range(NTT_COEFFICIENT_BYTES):
                stage[byte_index] = memory.read8(u64(source + byte_index))
            index = self._index % NTT_SIZE
            polynomial[index] = int.from_bytes(stage, "little") % self._modulus
            self._index = (self._index + 1) % NTT_SIZE

    def store(self, address: int, memory: SparseAddressSpace) -> None:
        """Store 256 result uint32s with device-read-before-write ordering."""

        address = self._cell(address, label="NTT destination address")
        memory = self._memory(memory)
        self._index = 0
        if _ordinary_transfer(self, memory, address):
            payload = _TRANSFER_FORMAT.pack(*(value & 0xFFFF_FFFF for value in self._result))
            memory.write_bytes(address, payload)
            return
        for coefficient in range(NTT_SIZE):
            destination = u64(address + coefficient * NTT_COEFFICIENT_BYTES)
            value = self._result[self._index % NTT_SIZE]
            for byte_index in range(NTT_COEFFICIENT_BYTES):
                byte = (value >> (byte_index * 8)) & 0xFF
                if byte_index == NTT_COEFFICIENT_BYTES - 1:
                    self._index = (self._index + 1) % NTT_SIZE
                memory.write8(u64(destination + byte_index), byte)

    def forward(self) -> None:
        """Synchronously transform polynomial A into the result buffer."""

        self._execute("forward")

    def inverse(self) -> None:
        """Synchronously inverse-transform polynomial A into result."""

        self._execute("inverse")

    def pointwise_multiply(self) -> None:
        """Synchronously multiply A and B coefficient-by-coefficient."""

        self._execute("multiply")

    def pointwise_add(self) -> None:
        """Synchronously add A and B coefficient-by-coefficient."""

        self._execute("add")

    def reset(self) -> None:
        """Restore the hosted device's construction state."""

        self._modulus = NTT_DEFAULT_MODULUS
        self._index = 0
        self._poly_a[:] = [0] * NTT_SIZE
        self._poly_b[:] = [0] * NTT_SIZE
        self._result[:] = [0] * NTT_SIZE
        self._busy = False
        self._done = False
        self._load_a_stage[:] = bytes(NTT_COEFFICIENT_BYTES)
        self._load_b_stage[:] = bytes(NTT_COEFFICIENT_BYTES)
        self._roots = find_ntt_roots(self._modulus)

    def _execute(self, operation: str) -> None:
        if self._busy:
            return
        self._busy = True
        self._done = False
        if self._roots is None:
            self._busy = False
            self._done = True
            return
        if operation == "forward":
            result = ntt_forward(
                self._poly_a,
                self._modulus,
                roots=self._roots,
            )
        elif operation == "inverse":
            result = ntt_inverse(
                self._poly_a,
                self._modulus,
                roots=self._roots,
            )
        elif operation == "multiply":
            result = ntt_pointwise_multiply(
                self._poly_a,
                self._poly_b,
                self._modulus,
            )
        elif operation == "add":
            result = ntt_pointwise_add(
                self._poly_a,
                self._poly_b,
                self._modulus,
            )
        else:
            raise AssertionError(f"unknown hosted NTT operation {operation!r}")
        self._result[:] = result
        self._busy = False
        self._done = True

    @staticmethod
    def _memory(memory: SparseAddressSpace) -> SparseAddressSpace:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("NTT memory must be a SparseAddressSpace")
        return memory

    @staticmethod
    def _cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer")
        return u64(value)


_SERVICE_TYPE = HostedNTTService
_SERVICE_METHODS = tuple(
    (name, HostedNTTService.__dict__[name])
    for name in ("load", "store", "_cell", "_memory")
)
_SERVICE_GLOBALS = tuple(
    (name, globals()[name])
    for name in ("u64", "NTT_SIZE", "NTT_COEFFICIENT_BYTES")
)


__all__ = [
    "HostedNTTService",
    "NTT_STATUS_BUSY",
    "NTT_STATUS_DONE",
    "NTT_STATUS_IDLE",
]
