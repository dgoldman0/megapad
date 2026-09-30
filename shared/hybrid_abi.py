"""Immutable values for the bounded integer-routine hybrid ABI.

These values validate finite host inputs only. They do not import an engine,
grant access to guest memory, establish lease liveness, or execute code.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from enum import Enum

from shared.cells import MASK64


HYBRID_ABI = "megapad.hybrid.integer-routine"
HYBRID_ABI_VERSION = 1
MAX_CODE_BYTES = 1 << 20
MAX_TOTAL_CODE_BYTES = 16 << 20
MAX_MANIFEST_BYTES = 1 << 20
MAX_ROUTINES = 64
MAX_BUFFER_RULES = 16
MAX_SIGNATURE_CELLS = 8
MAX_RETURN_STACK_CELLS = 8192
MAX_CONTROL_BYTES = 4 << 20
MAX_CALL_INSTRUCTIONS = 1_000_000
MAX_DISPATCH_INSTRUCTIONS = 10_000_000
CODE_ALIGNMENT = 16
CELL_BYTES = 8


def _integer(value: int, label: str, minimum: int, maximum: int) -> int:
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact integer")
    if not minimum <= value <= maximum:
        raise ValueError(f"{label} must be in {minimum}..{maximum}")
    return value


def _span(base: int, size: int, label: str, *, empty: bool = False) -> None:
    _integer(base, f"{label} base", 0, MASK64)
    _integer(size, f"{label} size", 0 if empty else 1, MASK64)
    if size and size - 1 > MASK64 - base:
        raise ValueError(f"{label} span wraps the uint64 address space")


def _version(abi: str, version: int) -> None:
    if type(abi) is not str:
        raise TypeError("ABI identity must be a string")
    if abi != HYBRID_ABI:
        raise ValueError("unsupported hybrid ABI identity")
    _integer(version, "ABI version", 1, HYBRID_ABI_VERSION)


def _name(name: str) -> None:
    if type(name) is not str:
        raise TypeError("routine name must be a string")
    if not 1 <= len(name) <= 127 or any(not 0x21 <= ord(char) <= 0x7E for char in name):
        raise ValueError("routine name must contain 1..127 printable nonwhitespace ASCII bytes")


class BufferAccessV1(str, Enum):
    READ = "read"
    WRITE = "write"
    READ_WRITE = "read_write"


def _access(value: BufferAccessV1 | str) -> BufferAccessV1:
    if isinstance(value, BufferAccessV1):
        return value
    if type(value) is not str:
        raise TypeError("buffer access must be a string")
    try:
        return BufferAccessV1(value)
    except ValueError as exc:
        raise ValueError("buffer access must be read, write, or read_write") from exc


@dataclass(frozen=True, slots=True, kw_only=True)
class BufferSpanV1:
    """Resolved numerical permission; a bridge still qualifies its geometry."""

    base: int
    size: int
    access: BufferAccessV1 | str

    def __post_init__(self) -> None:
        _span(self.base, self.size, "buffer", empty=True)
        object.__setattr__(self, "access", _access(self.access))

    @property
    def limit(self) -> int:
        return self.base + self.size


@dataclass(frozen=True, slots=True, kw_only=True)
class BufferRuleV1:
    address_argument: int
    length_argument: int
    element_bytes: int
    max_bytes: int
    access: BufferAccessV1 | str

    def __post_init__(self) -> None:
        _integer(self.address_argument, "address argument", 0, MAX_SIGNATURE_CELLS - 1)
        _integer(self.length_argument, "length argument", 0, MAX_SIGNATURE_CELLS - 1)
        _integer(self.element_bytes, "element bytes", 1, MASK64)
        _integer(self.max_bytes, "maximum buffer bytes", 0, MASK64)
        object.__setattr__(self, "access", _access(self.access))

    def resolve(self, arguments: Sequence[int]) -> BufferSpanV1:
        """Evaluate a span from original unsigned argument cells, without wrap.

        Empty spans retain their pointer and grant no bytes. Whether that
        pointer must name ordinary memory remains the bridge's preflight rule.
        """

        if not isinstance(arguments, Sequence):
            raise TypeError("arguments must be a sequence of uint64 cells")
        if not 0 <= len(arguments) <= MAX_SIGNATURE_CELLS:
            raise ValueError("arguments must contain at most eight cells")
        if max(self.address_argument, self.length_argument) >= len(arguments):
            raise ValueError("buffer rule argument is outside the input signature")
        for index in range(len(arguments)):
            _integer(arguments[index], f"argument {index}", 0, MASK64)
        base = arguments[self.address_argument]
        count = arguments[self.length_argument]
        if count == 0:
            return BufferSpanV1(base=base, size=0, access=self.access)
        if count > MASK64 // self.element_bytes:
            raise ValueError("buffer length multiplication overflows uint64")
        size = count * self.element_bytes
        if size > self.max_bytes:
            raise ValueError("buffer length exceeds maximum buffer bytes")
        return BufferSpanV1(base=base, size=size, access=self.access)


def _routine_values(value: object) -> None:
    _name(value.name)
    if type(value.code) is not bytes:
        raise TypeError("routine code must be immutable bytes")
    _integer(len(value.code), "code image bytes", 1, MAX_CODE_BYTES)
    _integer(value.entry_offset, "entry offset", 0, len(value.code) - 1)
    _integer(value.input_cells, "input cells", 0, MAX_SIGNATURE_CELLS)
    _integer(value.output_cells, "output cells", 0, MAX_SIGNATURE_CELLS)
    if type(value.buffers) is not tuple:
        raise TypeError("buffer rules must be an immutable tuple")
    if len(value.buffers) > MAX_BUFFER_RULES:
        raise ValueError("a routine may declare at most 16 buffer rules")
    for rule in value.buffers:
        if type(rule) is not BufferRuleV1:
            raise TypeError("buffer rules must be BufferRuleV1 values")
        if max(rule.address_argument, rule.length_argument) >= value.input_cells:
            raise ValueError("buffer rule argument is outside the input signature")
    _integer(value.max_instructions, "per-call instructions", 1, MAX_CALL_INSTRUCTIONS)
    _integer(value.return_stack_cells, "return stack cells", 1, MAX_RETURN_STACK_CELLS)
    _version(value.abi, value.version)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineImageV1:
    """Validated unregistered input image; publication may add sealed padding."""

    name: str
    code: bytes
    entry_offset: int
    input_cells: int
    output_cells: int
    buffers: tuple[BufferRuleV1, ...]
    max_instructions: int
    return_stack_cells: int
    abi: str = HYBRID_ABI
    version: int = HYBRID_ABI_VERSION

    def __post_init__(self) -> None:
        _routine_values(self)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineManifestV1:
    dispatch_instruction_limit: int
    routines: tuple[RoutineImageV1, ...]
    abi: str = HYBRID_ABI
    version: int = HYBRID_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version)
        _integer(self.dispatch_instruction_limit, "dispatch instructions", 1,
                 MAX_DISPATCH_INSTRUCTIONS)
        if type(self.routines) is not tuple:
            raise TypeError("manifest routines must be an immutable tuple")
        if len(self.routines) > MAX_ROUTINES:
            raise ValueError("a session may declare at most 64 routines")
        names: set[str] = set()
        total = 0
        for routine in self.routines:
            if type(routine) is not RoutineImageV1:
                raise TypeError("manifest routines must be RoutineImageV1 values")
            key = routine.name.upper()
            if key in names:
                raise ValueError(f"duplicate routine name: {routine.name}")
            names.add(key)
            # Publication pads each executable image to a cache-line boundary;
            # reserve that padding in the aggregate limit before publication.
            total += ((len(routine.code) + CODE_ALIGNMENT - 1) // CODE_ALIGNMENT) * CODE_ALIGNMENT
        if total > MAX_TOTAL_CODE_BYTES:
            raise ValueError("total code image bytes exceed 16 MiB")


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineDeclarationV1:
    """Sealed registration values, with adapter-owned opaque lease identities.

    Holding these values does not prove that a lease is live. The composition
    owner revalidates exact identities, generations, and bytes before entry.
    """

    name: str
    session_nonce: object
    registration_nonce: object
    allocation_lease: object
    allocation_generation: int
    control_lease: object
    control_generation: int
    body_base: int
    body_size: int
    code_base: int
    code: bytes
    entry_offset: int
    input_cells: int
    output_cells: int
    buffers: tuple[BufferRuleV1, ...]
    stack_base: int
    return_stack_cells: int
    max_instructions: int
    dispatch_instruction_limit: int
    abi: str = HYBRID_ABI
    version: int = HYBRID_ABI_VERSION

    def __post_init__(self) -> None:
        _routine_values(self)
        for label in ("session_nonce", "registration_nonce", "allocation_lease", "control_lease"):
            if getattr(self, label) is None:
                raise ValueError(f"{label} must identify its owner")
        _integer(self.allocation_generation, "allocation generation", 1, MASK64)
        _integer(self.control_generation, "control generation", 1, MASK64)
        _integer(self.dispatch_instruction_limit, "dispatch instructions", 1,
                 MAX_DISPATCH_INSTRUCTIONS)
        _span(self.body_base, self.body_size, "body allocation")
        _span(self.code_base, len(self.code), "code")
        if self.code_base % CODE_ALIGNMENT or len(self.code) % CODE_ALIGNMENT:
            raise ValueError("sealed code base and size must be aligned to 16 bytes")
        if not (
            self.body_base <= self.code_base
            and self.code_base + len(self.code) <= self.body_base + self.body_size
        ):
            raise ValueError("sealed code must lie inside its complete body allocation")
        _span(self.stack_base, self.stack_size, "private stack")
        if self.stack_base % CELL_BYTES:
            raise ValueError("private stack base must be cell aligned")
        if max(self.stack_base, self.body_base) < min(
            self.stack_base + self.stack_size, self.body_base + self.body_size,
        ):
            raise ValueError("private stack must be disjoint from the body allocation")

    @property
    def code_size(self) -> int:
        return len(self.code)

    @property
    def entry_pc(self) -> int:
        return self.code_base + self.entry_offset

    @property
    def stack_size(self) -> int:
        return self.return_stack_cells * CELL_BYTES


class MachineExitKindV1(str, Enum):
    RETURNED = "returned"
    INSTRUCTION_LIMIT = "instruction_limit"
    UNSUPPORTED_INSTRUCTION = "unsupported_instruction"
    REJECTED_ACCESS = "rejected_access"
    DECODE_FAULT = "decode_fault"
    INVALID_RETURN = "invalid_return"
    CANCELLED = "cancelled"


@dataclass(frozen=True, slots=True, kw_only=True)
class MachineRoutineResultV1:
    exit_kind: MachineExitKindV1 | str
    instructions: int
    cycles: int
    entry_pc: int
    pc: int
    outputs: tuple[int, ...] = ()
    instruction_pc: int | None = None
    access_address: int | None = None
    access_width: int | None = None
    access_operation: str | None = None
    trap_id: int = -1
    detail: str = ""
    abi: str = HYBRID_ABI
    version: int = HYBRID_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version)
        if not isinstance(self.exit_kind, MachineExitKindV1):
            if type(self.exit_kind) is not str:
                raise TypeError("machine exit kind must be a string")
            object.__setattr__(self, "exit_kind", MachineExitKindV1(self.exit_kind))
        _integer(self.instructions, "completed instructions", 0, MAX_CALL_INSTRUCTIONS)
        _integer(self.cycles, "completed cycles", 0, MASK64)
        _integer(self.entry_pc, "entry PC", 0, MASK64)
        _integer(self.pc, "PC", 0, MASK64)
        if type(self.outputs) is not tuple:
            raise TypeError("machine outputs must be an immutable tuple")
        if len(self.outputs) > MAX_SIGNATURE_CELLS:
            raise ValueError("machine outputs may contain at most eight cells")
        for output in self.outputs:
            _integer(output, "output cell", 0, MASK64)
        if self.exit_kind != MachineExitKindV1.RETURNED and self.outputs:
            raise ValueError("failed machine execution cannot publish outputs")
        for label in ("instruction_pc", "access_address"):
            value = getattr(self, label)
            if value is not None:
                _integer(value, label, 0, MASK64)
        _integer(self.trap_id, "trap ID", -1, MASK64)
        if self.access_width is not None:
            _integer(self.access_width, "access width", 1, 8)
            if self.access_width not in (1, 2, 4, 8):
                raise ValueError("access width must be 1, 2, 4, or 8")
        if self.access_operation is not None and type(self.access_operation) is not str:
            raise TypeError("access_operation must be a string or None")
        if type(self.detail) is not str:
            raise TypeError("detail must be a string")


__all__ = [
    "HYBRID_ABI", "HYBRID_ABI_VERSION", "MAX_CODE_BYTES", "MAX_TOTAL_CODE_BYTES",
    "MAX_MANIFEST_BYTES", "MAX_ROUTINES", "MAX_BUFFER_RULES", "MAX_SIGNATURE_CELLS",
    "MAX_RETURN_STACK_CELLS", "MAX_CONTROL_BYTES", "MAX_CALL_INSTRUCTIONS",
    "MAX_DISPATCH_INSTRUCTIONS", "CODE_ALIGNMENT", "CELL_BYTES", "BufferAccessV1",
    "BufferSpanV1", "BufferRuleV1", "RoutineImageV1", "RoutineManifestV1",
    "RoutineDeclarationV1", "MachineExitKindV1", "MachineRoutineResultV1",
]
