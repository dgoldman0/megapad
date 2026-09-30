"""Immutable values for the bounded integer-routine hybrid ABI.

These values validate finite host inputs only. They do not import an engine,
grant access to guest memory, establish lease liveness, or execute code.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING

from shared.cells import MASK64

if TYPE_CHECKING:
    from shared.hybrid_closed import ClosedPolicyV3


HYBRID_ABI = "megapad.hybrid.integer-routine"
HYBRID_ABI_VERSION = 1
HYBRID_CALLBACK_ABI_VERSION = 2
HYBRID_CLOSED_ABI_VERSION = 3
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
MAX_CALLBACK_EXPORTS = 64
MAX_CALLBACK_SITES = 16
MAX_DISPATCH_CALLBACKS = 1024
MAX_CALLBACK_SEMANTIC_STEPS = 4096
MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS = 65536


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


def _version(abi: str, version: int, *, expected: int = HYBRID_ABI_VERSION) -> None:
    if type(abi) is not str:
        raise TypeError("ABI identity must be a string")
    if abi != HYBRID_ABI:
        raise ValueError("unsupported hybrid ABI identity")
    _integer(version, "ABI version", expected, expected)


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


def _routine_values(value: object, *, version: int = HYBRID_ABI_VERSION) -> None:
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
        if version == HYBRID_CLOSED_ABI_VERSION:
            # V3 containers revalidate nested values before using their fields.
            BufferRuleV1.__post_init__(rule)
        if max(rule.address_argument, rule.length_argument) >= value.input_cells:
            raise ValueError("buffer rule argument is outside the input signature")
    _integer(value.max_instructions, "per-call instructions", 1, MAX_CALL_INSTRUCTIONS)
    _integer(value.return_stack_cells, "return stack cells", 1, MAX_RETURN_STACK_CELLS)
    _version(value.abi, value.version, expected=version)


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
        self._validate_declaration()

    def _validate_declaration(self, *, version: int = HYBRID_ABI_VERSION) -> None:
        _routine_values(self, version=version)
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
        self._validate_result()

    def _validate_result(self, *, version: int = HYBRID_ABI_VERSION,
                         exit_type: type[Enum] = MachineExitKindV1) -> None:
        _version(self.abi, self.version, expected=version)
        if not isinstance(self.exit_kind, exit_type):
            if type(self.exit_kind) is not str:
                raise TypeError("machine exit kind must be a string")
            object.__setattr__(self, "exit_kind", exit_type(self.exit_kind))
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


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackExportV2:
    """A leaf signature, never authority to call a word or host function.

    The semantic owner independently binds the exact original canonical Word.
    Names and IDs in this value cannot establish that identity or its lifetime.
    """

    export_id: int
    name: str
    input_cells: int
    output_cells: int
    max_semantic_steps: int = 1
    effect: str = "integer_leaf"
    abi: str = HYBRID_ABI
    version: int = HYBRID_CALLBACK_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_CALLBACK_ABI_VERSION)
        _integer(self.export_id, "callback export ID", 0, MAX_CALLBACK_EXPORTS - 1)
        _name(self.name)
        if self.name not in ("MIN", "MAX", "ABS", "AND", "OR", "XOR"):
            raise ValueError("callback export must name a canonical integer leaf")
        _integer(self.input_cells, "callback input cells", 0, MAX_SIGNATURE_CELLS)
        _integer(self.output_cells, "callback output cells", 0, MAX_SIGNATURE_CELLS)
        if (self.input_cells, self.output_cells) != ((1, 1) if self.name == "ABS" else (2, 1)):
            raise ValueError("callback arity does not match its canonical integer leaf")
        # _execute_top charges one tick then invokes these total primitives;
        # none returns Invoke, enters a service, or performs nested dispatch.
        _integer(self.max_semantic_steps, "integer leaf semantic steps", 1, 1)
        if type(self.effect) is not str:
            raise TypeError("callback effect must be a string")
        if self.effect != "integer_leaf":
            raise ValueError("callback effect must be integer_leaf")


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackSiteV2:
    """Numerical metadata for an unprefixed two-byte CALL and one-byte RET.

    The native publisher still proves encodings, instruction boundaries and
    dynamic call provenance against the complete sealed image.
    """

    call_offset: int
    stub_offset: int
    export: CallbackExportV2
    abi: str = HYBRID_ABI
    version: int = HYBRID_CALLBACK_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_CALLBACK_ABI_VERSION)
        _integer(self.call_offset, "callback call offset", 0, MAX_CODE_BYTES - 2)
        _integer(self.stub_offset, "callback stub offset", 0, MAX_CODE_BYTES - 1)
        if self.call_offset <= self.stub_offset < self.call_offset + 2:
            raise ValueError("callback call and stub byte spans must be disjoint")
        if type(self.export) is not CallbackExportV2:
            raise TypeError("callback export must be a CallbackExportV2 value")


def _callback_sites(value: object, *, site_type: type = CallbackSiteV2) -> None:
    if type(value.callbacks) is not tuple:
        raise TypeError("callback sites must be an immutable tuple")
    if len(value.callbacks) > MAX_CALLBACK_SITES:
        raise ValueError("a routine may declare at most 16 callback sites")
    exports: dict[int, CallbackExportV2] = {}
    occupied: set[int] = set()
    for site in value.callbacks:
        if type(site) is not site_type:
            raise TypeError(f"callback sites must be {site_type.__name__} values")
        if site_type is not CallbackSiteV2:
            site_type.__post_init__(site)
        if site.call_offset + 2 > len(value.code) or site.stub_offset >= len(value.code):
            raise ValueError("complete callback call and stub must lie inside the code image")
        offsets = (site.call_offset, site.call_offset + 1, site.stub_offset)
        if any(offset in occupied for offset in offsets):
            raise ValueError("callback site byte spans must not overlap or repeat")
        occupied.update(offsets)
        previous = exports.setdefault(site.export.export_id, site.export)
        if previous != site.export:
            raise ValueError("callback export ID has conflicting descriptors")


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineImageV2(RoutineImageV1):
    """Unpublished callback image; the v1 loader does not admit this type."""

    callbacks: tuple[CallbackSiteV2, ...]
    version: int = HYBRID_CALLBACK_ABI_VERSION

    def __post_init__(self) -> None:
        _routine_values(self, version=HYBRID_CALLBACK_ABI_VERSION)
        _callback_sites(self)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineManifestV2:
    """Unpublished images and leaf descriptors, without callback authority."""

    dispatch_instruction_limit: int
    dispatch_callback_limit: int
    dispatch_callback_semantic_limit: int
    exports: tuple[CallbackExportV2, ...]
    routines: tuple[RoutineImageV2, ...]
    abi: str = HYBRID_ABI
    version: int = HYBRID_CALLBACK_ABI_VERSION

    def __post_init__(self) -> None:
        self._validate_manifest()

    def _validate_manifest(
        self, *, version: int = HYBRID_CALLBACK_ABI_VERSION,
        export_type: type = CallbackExportV2, routine_type: type = RoutineImageV2,
    ) -> None:
        _version(self.abi, self.version, expected=version)
        _integer(self.dispatch_instruction_limit, "dispatch instructions", 1,
                 MAX_DISPATCH_INSTRUCTIONS)
        _integer(self.dispatch_callback_limit, "dispatch callback requests", 1,
                 MAX_DISPATCH_CALLBACKS)
        _integer(self.dispatch_callback_semantic_limit, "dispatch callback semantic steps", 1,
                 MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)
        if type(self.exports) is not tuple:
            raise TypeError("manifest exports must be an immutable tuple")
        if len(self.exports) > MAX_CALLBACK_EXPORTS:
            raise ValueError("a manifest may declare at most 64 callback exports")
        exports = {}
        for export in self.exports:
            if type(export) is not export_type:
                raise TypeError(f"manifest exports must be {export_type.__name__} values")
            if export_type is not CallbackExportV2:
                export_type.__post_init__(export)
            if export.export_id in exports:
                raise ValueError(f"duplicate callback export ID: {export.export_id}")
            exports[export.export_id] = export
        if type(self.routines) is not tuple:
            raise TypeError("manifest routines must be an immutable tuple")
        if len(self.routines) > MAX_ROUTINES:
            raise ValueError("a session may declare at most 64 routines")
        names: set[str] = set()
        total = 0
        for routine in self.routines:
            if type(routine) is not routine_type:
                raise TypeError(f"manifest routines must be {routine_type.__name__} values")
            if routine_type is not RoutineImageV2:
                routine_type.__post_init__(routine)
            key = routine.name.upper()
            if key in names:
                raise ValueError(f"duplicate routine name: {routine.name}")
            names.add(key)
            total += ((len(routine.code) + CODE_ALIGNMENT - 1) // CODE_ALIGNMENT) * CODE_ALIGNMENT
            for site in routine.callbacks:
                descriptor = exports.get(site.export.export_id)
                if descriptor is None:
                    raise ValueError(f"undeclared callback export ID: {site.export.export_id}")
                if descriptor != site.export:
                    raise ValueError("callback export ID has conflicting descriptors")
        if total > MAX_TOTAL_CODE_BYTES:
            raise ValueError("total code image bytes exceed 16 MiB")


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineDeclarationV2(RoutineDeclarationV1):
    """Sealed v2 metadata; allocation/export/token authority stays external."""

    callbacks: tuple[CallbackSiteV2, ...]
    dispatch_callback_limit: int = MAX_DISPATCH_CALLBACKS
    dispatch_callback_semantic_limit: int = MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS
    version: int = HYBRID_CALLBACK_ABI_VERSION

    def __post_init__(self) -> None:
        self._validate_declaration(version=HYBRID_CALLBACK_ABI_VERSION)
        _callback_sites(self)
        _integer(self.dispatch_callback_limit, "dispatch callback requests", 1,
                 MAX_DISPATCH_CALLBACKS)
        _integer(self.dispatch_callback_semantic_limit, "dispatch callback semantic steps", 1,
                 MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackRequestV2:
    """Copyable callback observation, not a native continuation capability."""

    invocation_id: int
    sequence: int
    site: CallbackSiteV2
    arguments: tuple[int, ...]
    abi: str = HYBRID_ABI
    version: int = HYBRID_CALLBACK_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_CALLBACK_ABI_VERSION)
        _integer(self.invocation_id, "invocation ID", 1, MASK64)
        _integer(self.sequence, "callback request sequence", 1, MAX_DISPATCH_CALLBACKS)
        if type(self.site) is not CallbackSiteV2:
            raise TypeError("callback request site must be a CallbackSiteV2 value")
        if type(self.arguments) is not tuple:
            raise TypeError("callback arguments must be an immutable tuple")
        if len(self.arguments) != self.site.export.input_cells:
            raise ValueError("callback argument count does not match its export")
        for argument in self.arguments:
            _integer(argument, "callback argument cell", 0, MASK64)


class MachineExitKindV2(str, Enum):
    RETURNED = "returned"
    INSTRUCTION_LIMIT = "instruction_limit"
    UNSUPPORTED_INSTRUCTION = "unsupported_instruction"
    REJECTED_ACCESS = "rejected_access"
    DECODE_FAULT = "decode_fault"
    INVALID_RETURN = "invalid_return"
    CANCELLED = "cancelled"
    CALLBACK_REQUEST = "callback_request"
    CALLBACK_LIMIT = "callback_limit"
    INVALID_CALLBACK = "invalid_callback"


@dataclass(frozen=True, slots=True, kw_only=True)
class MachineSegmentResultV2(MachineRoutineResultV1):
    """One settled segment, with distinct deltas and invocation totals.

    A callback request is nonterminal and has no final machine outputs. The
    associated native continuation token is intentionally not a shared value.
    """

    invocation_id: int
    invocation_instructions: int
    invocation_cycles: int
    callback: CallbackRequestV2 | None = None
    exit_kind: MachineExitKindV2 | str
    version: int = HYBRID_CALLBACK_ABI_VERSION

    def __post_init__(self) -> None:
        self._validate_segment()

    def _validate_segment(
        self, *, version: int = HYBRID_CALLBACK_ABI_VERSION,
        request_type: type = CallbackRequestV2,
    ) -> None:
        self._validate_result(version=version, exit_type=MachineExitKindV2)
        _integer(self.invocation_id, "invocation ID", 1, MASK64)
        _integer(self.invocation_instructions, "invocation instructions", 0,
                 MAX_CALL_INSTRUCTIONS)
        _integer(self.invocation_cycles, "invocation cycles", 0, MASK64)
        if self.instructions > self.invocation_instructions or self.cycles > self.invocation_cycles:
            raise ValueError("segment counters cannot exceed invocation totals")
        for instructions, cycles in (
            (self.instructions, self.cycles),
            (self.invocation_instructions - self.instructions,
             self.invocation_cycles - self.cycles),
        ):
            if cycles < instructions or (instructions == 0 and cycles != 0):
                raise ValueError("completed instruction and cycle counters are inconsistent")
        if self.exit_kind is MachineExitKindV2.RETURNED and self.instructions == 0:
            raise ValueError("returned exit requires a completed RET instruction")
        if self.exit_kind is MachineExitKindV2.CALLBACK_REQUEST:
            if type(self.callback) is not request_type:
                raise TypeError(f"callback request exit requires a {request_type.__name__} value")
            if request_type is not CallbackRequestV2:
                request_type.__post_init__(self.callback)
            if self.callback.invocation_id != self.invocation_id:
                raise ValueError("callback request belongs to a different invocation")
            if self.instructions == 0:
                raise ValueError("callback request requires a completed CALL instruction")
            if self.callback.sequence > self.invocation_instructions:
                raise ValueError("callback request sequence exceeds completed calls")
        elif self.callback is not None:
            raise ValueError("only callback request exits may carry a callback request")


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackExportV3(CallbackExportV2):
    """Versioned signature; a closed Word's proof and authority stay external."""

    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_CLOSED_ABI_VERSION)
        if type(self.effect) is not str:
            raise TypeError("callback effect must be a string")
        if self.effect == "integer_leaf":
            # Reuse the frozen leaf catalog and bounds without broadening v2.
            CallbackExportV2(
                export_id=self.export_id, name=self.name,
                input_cells=self.input_cells, output_cells=self.output_cells,
                max_semantic_steps=self.max_semantic_steps, effect=self.effect,
                abi=self.abi,
            )
        elif self.effect == "closed_integer_colon":
            _integer(self.export_id, "callback export ID", 0, MAX_CALLBACK_EXPORTS - 1)
            _name(self.name)
            _integer(self.input_cells, "callback input cells", 0, MAX_SIGNATURE_CELLS)
            _integer(self.output_cells, "callback output cells", 0, MAX_SIGNATURE_CELLS)
            _integer(self.max_semantic_steps, "closed callback semantic steps", 1,
                     MAX_CALLBACK_SEMANTIC_STEPS)
        else:
            raise ValueError("callback effect must be integer_leaf or closed_integer_colon")


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackSiteV3(CallbackSiteV2):
    export: CallbackExportV3
    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_CLOSED_ABI_VERSION)
        _integer(self.call_offset, "callback call offset", 0, MAX_CODE_BYTES - 2)
        _integer(self.stub_offset, "callback stub offset", 0, MAX_CODE_BYTES - 1)
        if self.call_offset <= self.stub_offset < self.call_offset + 2:
            raise ValueError("callback call and stub byte spans must be disjoint")
        if type(self.export) is not CallbackExportV3:
            raise TypeError("callback export must be a CallbackExportV3 value")
        CallbackExportV3.__post_init__(self.export)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineImageV3(RoutineImageV1):
    callbacks: tuple[CallbackSiteV3, ...]
    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        _routine_values(self, version=HYBRID_CLOSED_ABI_VERSION)
        _callback_sites(self, site_type=CallbackSiteV3)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineDeclarationV3(RoutineDeclarationV1):
    callbacks: tuple[CallbackSiteV3, ...]
    dispatch_callback_limit: int = MAX_DISPATCH_CALLBACKS
    dispatch_callback_semantic_limit: int = MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS
    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        self._validate_declaration(version=HYBRID_CLOSED_ABI_VERSION)
        _callback_sites(self, site_type=CallbackSiteV3)
        _integer(self.dispatch_callback_limit, "dispatch callback requests", 1,
                 MAX_DISPATCH_CALLBACKS)
        _integer(self.dispatch_callback_semantic_limit, "dispatch callback semantic steps", 1,
                 MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineManifestV3(RoutineManifestV2):
    policies: tuple[ClosedPolicyV3, ...]
    exports: tuple[CallbackExportV3, ...]
    routines: tuple[RoutineImageV3, ...]
    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        from shared.hybrid_closed import ClosedPolicyV3, prove_policies

        self._validate_manifest(version=HYBRID_CLOSED_ABI_VERSION,
                                export_type=CallbackExportV3, routine_type=RoutineImageV3)
        if type(self.policies) is not tuple:
            raise TypeError("manifest policies must be an exact immutable tuple")
        if any(type(policy) is not ClosedPolicyV3 for policy in self.policies):
            raise TypeError("manifest policies must be ClosedPolicyV3 values")
        proofs = {proof.policy_id: proof for proof in prove_policies(self.policies)}
        policies = {policy.name: policy for policy in self.policies}
        routine_names = {routine.name.upper() for routine in self.routines}
        if any(policy.name.upper() in routine_names for policy in self.policies):
            raise ValueError("policy and machine routine names must not collide")
        for export in self.exports:
            if export.effect != "closed_integer_colon":
                continue
            policy = policies.get(export.name)
            if policy is None:
                raise ValueError("closed manifest export must name a declared policy")
            if (export.input_cells, export.output_cells) != (policy.input_cells, policy.output_cells):
                raise ValueError("closed export arity must match its declared policy")
            proofs[policy.policy_id].validate_signature(
                export.input_cells, export.output_cells, export.max_semantic_steps,
            )


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackRequestV3(CallbackRequestV2):
    site: CallbackSiteV3
    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_CLOSED_ABI_VERSION)
        _integer(self.invocation_id, "invocation ID", 1, MASK64)
        _integer(self.sequence, "callback request sequence", 1, MAX_DISPATCH_CALLBACKS)
        if type(self.site) is not CallbackSiteV3:
            raise TypeError("callback request site must be a CallbackSiteV3 value")
        CallbackSiteV3.__post_init__(self.site)
        if type(self.arguments) is not tuple:
            raise TypeError("callback arguments must be an immutable tuple")
        if len(self.arguments) != self.site.export.input_cells:
            raise ValueError("callback argument count does not match its export")
        for argument in self.arguments:
            _integer(argument, "callback argument cell", 0, MASK64)


@dataclass(frozen=True, slots=True, kw_only=True)
class MachineSegmentResultV3(MachineSegmentResultV2):
    callback: CallbackRequestV3 | None = None
    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        self._validate_segment(version=HYBRID_CLOSED_ABI_VERSION,
                               request_type=CallbackRequestV3)


__all__ = [
    "HYBRID_ABI", "HYBRID_ABI_VERSION", "MAX_CODE_BYTES", "MAX_TOTAL_CODE_BYTES",
    "MAX_MANIFEST_BYTES", "MAX_ROUTINES", "MAX_BUFFER_RULES", "MAX_SIGNATURE_CELLS",
    "MAX_RETURN_STACK_CELLS", "MAX_CONTROL_BYTES", "MAX_CALL_INSTRUCTIONS",
    "MAX_DISPATCH_INSTRUCTIONS", "CODE_ALIGNMENT", "CELL_BYTES", "BufferAccessV1",
    "BufferSpanV1", "BufferRuleV1", "RoutineImageV1", "RoutineManifestV1",
    "RoutineDeclarationV1", "MachineExitKindV1", "MachineRoutineResultV1",
    "HYBRID_CALLBACK_ABI_VERSION", "MAX_CALLBACK_EXPORTS", "MAX_CALLBACK_SITES",
    "MAX_DISPATCH_CALLBACKS", "MAX_CALLBACK_SEMANTIC_STEPS",
    "MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS", "CallbackExportV2", "CallbackSiteV2",
    "RoutineImageV2", "RoutineManifestV2", "RoutineDeclarationV2", "CallbackRequestV2",
    "MachineExitKindV2", "MachineSegmentResultV2",
    "HYBRID_CLOSED_ABI_VERSION", "CallbackExportV3", "CallbackSiteV3",
    "RoutineImageV3", "RoutineDeclarationV3", "RoutineManifestV3",
    "CallbackRequestV3", "MachineSegmentResultV3",
]
