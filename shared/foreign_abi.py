"""Finite values and the adapter contract for task foreign operations.

These values grant no execution, memory, registration, or continuation authority.
Opaque references are retained without inspection or equality checks. An engine
and its adapter must validate their own issued identities, current leases,
one-shot transitions, and accounting continuity at every boundary.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Protocol, TypeAlias

from shared.cells import MASK64


FOREIGN_ABI = "megapad.hybrid.task-routine"
FOREIGN_ABI_VERSION = 1
MAX_SIGNATURE_CELLS = 8
MAX_GRANTS = 16
MAX_DEPTH = 8
MAX_ROOT_ENTRIES = 1024
MAX_CALLBACK_SITES = 16
MAX_INVOCATION_INSTRUCTIONS = 1_000_000
MAX_ROOT_INSTRUCTIONS = 10_000_000
MAX_CALLBACK_REQUESTS = 1024
MAX_CALLBACK_SEMANTIC_STEPS = 4096
MAX_ROOT_CALLBACK_SEMANTIC_STEPS = 65536
MAX_DETAIL_CHARS = 1024


def _integer(value: int, label: str, minimum: int, maximum: int) -> None:
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact integer")
    if not minimum <= value <= maximum:
        raise ValueError(f"{label} must be in {minimum}..{maximum}")


def _version(value: object) -> None:
    if type(value.abi) is not str:
        raise TypeError("ABI identity must be an exact string")
    if value.abi != FOREIGN_ABI:
        raise ValueError("unsupported foreign ABI identity")
    _integer(value.version, "ABI version", FOREIGN_ABI_VERSION, FOREIGN_ABI_VERSION)


def _exact(value: object, kind: type, label: str) -> None:
    if type(value) is not kind:
        raise TypeError(f"{label} must be an exact {kind.__name__} value")
    kind.__post_init__(value)


def _tuple(value: object, label: str, maximum: int) -> None:
    if type(value) is not tuple:
        raise TypeError(f"{label} must be an immutable exact tuple")
    if len(value) > maximum:
        raise ValueError(f"{label} may contain at most {maximum} values")


def _cells(value: tuple[int, ...], label: str) -> None:
    _tuple(value, label, MAX_SIGNATURE_CELLS)
    for cell in value:
        _integer(cell, f"{label} cell", 0, MASK64)


def _opaque(value: object, label: str) -> None:
    # Even bool(), repr(), hash(), equality and attribute lookup can invoke
    # caller code. Identity with None is the only check on an opaque reference.
    if value is None:
        raise TypeError(f"{label} must be a non-None opaque reference")


def _enum(value: object, kind: type[Enum], label: str) -> Enum:
    if type(value) is kind:
        return value
    if type(value) is not str:
        raise TypeError(f"{label} must be an exact string or {kind.__name__}")
    return kind(value)


class ForeignAccessV1(str, Enum):
    READ = "read"
    WRITE = "write"
    READ_WRITE = "read_write"


class ForeignStateV1(str, Enum):
    CALLBACK = "callback"
    RETURNED = "returned"
    YIELDED = "yielded"
    FAILED = "failed"


class ForeignFailureKindV1(str, Enum):
    INSTRUCTION_LIMIT = "instruction_limit"
    CALLBACK_LIMIT = "callback_limit"
    UNSUPPORTED_INSTRUCTION = "unsupported_instruction"
    REJECTED_ACCESS = "rejected_access"
    DECODE_FAULT = "decode_fault"
    INVALID_RETURN = "invalid_return"
    INVALID_REPLY = "invalid_reply"
    STALE_REGISTRATION = "stale_registration"
    PROFILE_REJECTED = "profile_rejected"


@dataclass(frozen=True, slots=True, kw_only=True)
class ForeignSignatureV1:
    input_cells: int
    output_cells: int
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self)
        _integer(self.input_cells, "input cells", 0, MAX_SIGNATURE_CELLS)
        _integer(self.output_cells, "output cells", 0, MAX_SIGNATURE_CELLS)


@dataclass(frozen=True, slots=True, kw_only=True)
class ForeignSpanV1:
    """Numerical permission only; zero-sized spans authorize no bytes."""

    base: int
    size: int
    access: ForeignAccessV1 | str
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self)
        _integer(self.base, "span base", 0, MASK64)
        _integer(self.size, "span size", 0, MASK64)
        if self.size and self.size - 1 > MASK64 - self.base:
            raise ValueError("span wraps the uint64 address space")
        object.__setattr__(self, "access", _enum(self.access, ForeignAccessV1, "access"))

    @property
    def limit(self) -> int:
        return self.base + self.size


def _grants(value: tuple[ForeignSpanV1, ...]) -> None:
    _tuple(value, "grants", MAX_GRANTS)
    for grant in value:
        _exact(grant, ForeignSpanV1, "grant")


@dataclass(frozen=True, slots=True, kw_only=True, eq=False)
class ForeignOperationV1:
    """Description of an operation; the registration's authority stays external."""

    registration: object = field(repr=False)
    signature: ForeignSignatureV1
    machine_grants: tuple[ForeignSpanV1, ...] = ()
    max_instructions: int = MAX_INVOCATION_INSTRUCTIONS
    max_callbacks: int = MAX_CALLBACK_REQUESTS
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self)
        _opaque(self.registration, "registration")
        _exact(self.signature, ForeignSignatureV1, "signature")
        _grants(self.machine_grants)
        _integer(self.max_instructions, "invocation instruction limit", 1,
                 MAX_INVOCATION_INSTRUCTIONS)
        _integer(self.max_callbacks, "invocation callback limit", 0, MAX_CALLBACK_REQUESTS)


@dataclass(frozen=True, slots=True, kw_only=True, eq=False)
class ForeignExportV1:
    """Captured-target description, with semantic grants separate from machine grants."""

    export: object = field(repr=False)
    signature: ForeignSignatureV1
    task_grants: tuple[ForeignSpanV1, ...] = ()
    max_semantic_steps: int = MAX_CALLBACK_SEMANTIC_STEPS
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self)
        _opaque(self.export, "export")
        _exact(self.signature, ForeignSignatureV1, "signature")
        _grants(self.task_grants)
        _integer(self.max_semantic_steps, "callback semantic limit", 1,
                 MAX_CALLBACK_SEMANTIC_STEPS)


@dataclass(frozen=True, slots=True, kw_only=True)
class ForeignBudgetV1:
    """Remaining allowances, never a request to reset adapter-owned counters.

    A zero quantum permits a runnable yield before the first instruction.
    Exhausted instruction/callback allowances remain separate terminal limits.
    The semantic engine owns callback-local/root semantic fuel independently.
    """

    invocation_instructions_remaining: int
    root_instructions_remaining: int
    invocation_callbacks_remaining: int
    root_callbacks_remaining: int
    quantum_instructions: int
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self)
        for label, maximum in (
            ("invocation_instructions_remaining", MAX_INVOCATION_INSTRUCTIONS),
            ("root_instructions_remaining", MAX_ROOT_INSTRUCTIONS),
            ("invocation_callbacks_remaining", MAX_CALLBACK_REQUESTS),
            ("root_callbacks_remaining", MAX_CALLBACK_REQUESTS),
            ("quantum_instructions", MAX_INVOCATION_INSTRUCTIONS),
        ):
            _integer(getattr(self, label), label, 0, maximum)


@dataclass(frozen=True, slots=True, kw_only=True)
class ForeignReceiptV1:
    """One settled segment: deltas and exclusive invocation/root totals.

    Sequence is monotonic within the original root, not per invocation. The
    adapter publishes this receipt before constructing its event and retains
    it through cancellation. Cross-receipt continuity requires issued-owner
    validation; an independently constructed receipt is never evidence of work.
    """

    root_id: int
    invocation_id: int
    parent_invocation_id: int | None
    depth: int
    sequence: int
    invocation_started: bool
    root_entries: int
    state: ForeignStateV1 | str
    instructions: int
    cycles: int
    callback_requests: int
    invocation_instructions: int
    invocation_cycles: int
    invocation_callbacks: int
    root_instructions: int
    root_cycles: int
    root_callbacks: int
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self)
        for label in ("root_id", "invocation_id", "sequence"):
            _integer(getattr(self, label), label, 1, MASK64)
        _integer(self.depth, "depth", 1, MAX_DEPTH)
        if self.parent_invocation_id is None:
            if self.depth != 1:
                raise ValueError("a root invocation must have depth one")
        else:
            _integer(self.parent_invocation_id, "parent invocation ID", 1, MASK64)
            if self.depth == 1 or self.parent_invocation_id == self.invocation_id:
                raise ValueError("parent invocation and depth are inconsistent")
        if type(self.invocation_started) is not bool:
            raise TypeError("invocation_started must be an exact boolean")
        _integer(self.root_entries, "root entries", self.depth, MAX_ROOT_ENTRIES)
        if self.root_entries > self.sequence:
            raise ValueError("root entries cannot exceed the root segment sequence")
        object.__setattr__(self, "state", _enum(self.state, ForeignStateV1, "state"))
        for label, maximum in (
            ("instructions", MAX_INVOCATION_INSTRUCTIONS), ("cycles", MASK64),
            ("callback_requests", 1),
            ("invocation_instructions", MAX_INVOCATION_INSTRUCTIONS),
            ("invocation_cycles", MASK64), ("invocation_callbacks", MAX_CALLBACK_REQUESTS),
            ("root_instructions", MAX_ROOT_INSTRUCTIONS), ("root_cycles", MASK64),
            ("root_callbacks", MAX_CALLBACK_REQUESTS),
        ):
            _integer(getattr(self, label), label, 0, maximum)
        for delta, invocation, root in (
            (self.instructions, self.invocation_instructions, self.root_instructions),
            (self.cycles, self.invocation_cycles, self.root_cycles),
            (self.callback_requests, self.invocation_callbacks, self.root_callbacks),
        ):
            if not delta <= invocation <= root:
                raise ValueError("segment, invocation and root counters are inconsistent")
            if self.invocation_started and delta != invocation:
                raise ValueError("a started invocation cannot have prior own work")
        for instructions, cycles, callbacks in (
            (self.instructions, self.cycles, self.callback_requests),
            (self.invocation_instructions - self.instructions,
             self.invocation_cycles - self.cycles,
             self.invocation_callbacks - self.callback_requests),
            (self.root_instructions - self.invocation_instructions,
             self.root_cycles - self.invocation_cycles,
             self.root_callbacks - self.invocation_callbacks),
        ):
            if cycles < instructions or (instructions == 0 and cycles != 0):
                raise ValueError("instruction and cycle counters are inconsistent")
            if callbacks > instructions:
                raise ValueError("callback counts exceed completed instructions")
        if self.sequence == 1:
            if (not self.invocation_started or self.depth != 1 or self.root_entries != 1
                    or self.root_instructions != self.instructions
                    or self.root_cycles != self.cycles
                    or self.root_callbacks != self.callback_requests):
                raise ValueError("the first root receipt must describe its first invocation")
        if self.state is ForeignStateV1.CALLBACK:
            if self.instructions == 0 or self.callback_requests != 1:
                raise ValueError("callback receipt requires a completed CALL")
        elif self.state is ForeignStateV1.RETURNED:
            if self.instructions == 0 or self.callback_requests:
                raise ValueError("returned receipt requires a completed RET without a callback")
        elif self.state is ForeignStateV1.YIELDED and self.callback_requests:
            raise ValueError("runnable yield cannot also request a callback")

    @property
    def terminal(self) -> bool:
        return self.state in (ForeignStateV1.RETURNED, ForeignStateV1.FAILED)


def _event(value: object, state: ForeignStateV1) -> None:
    _version(value)
    _opaque(value.operation_token, "operation token")
    _exact(value.receipt, ForeignReceiptV1, "receipt")
    if value.receipt.state is not state:
        raise ValueError("event and receipt state differ")


@dataclass(frozen=True, slots=True, kw_only=True, eq=False)
class ForeignCallbackRequestV1:
    operation_token: object = field(repr=False)
    request_token: object = field(repr=False)
    receipt: ForeignReceiptV1
    request_sequence: int
    site: int
    export: ForeignExportV1
    arguments: tuple[int, ...]
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _event(self, ForeignStateV1.CALLBACK)
        _opaque(self.request_token, "request token")
        _integer(self.request_sequence, "request sequence", 1, MAX_CALLBACK_REQUESTS)
        if self.request_sequence != self.receipt.invocation_callbacks:
            raise ValueError("request sequence must match the invocation callback count")
        _integer(self.site, "callback site", 0, MAX_CALLBACK_SITES - 1)
        _exact(self.export, ForeignExportV1, "export")
        _cells(self.arguments, "callback arguments")
        if len(self.arguments) != self.export.signature.input_cells:
            raise ValueError("callback argument arity differs from the export signature")


@dataclass(frozen=True, slots=True, kw_only=True, eq=False)
class ForeignCompletedV1:
    operation_token: object = field(repr=False)
    receipt: ForeignReceiptV1
    outputs: tuple[int, ...]
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _event(self, ForeignStateV1.RETURNED)
        _cells(self.outputs, "outputs")


@dataclass(frozen=True, slots=True, kw_only=True, eq=False)
class ForeignRunnableYieldV1:
    operation_token: object = field(repr=False)
    receipt: ForeignReceiptV1
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _event(self, ForeignStateV1.YIELDED)


@dataclass(frozen=True, slots=True, kw_only=True, eq=False)
class ForeignFailedV1:
    operation_token: object = field(repr=False)
    receipt: ForeignReceiptV1
    kind: ForeignFailureKindV1 | str
    detail: str = ""
    instruction_pc: int | None = None
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _event(self, ForeignStateV1.FAILED)
        object.__setattr__(self, "kind", _enum(self.kind, ForeignFailureKindV1, "failure kind"))
        if type(self.detail) is not str:
            raise TypeError("detail must be an exact string")
        if len(self.detail) > MAX_DETAIL_CHARS:
            raise ValueError("detail exceeds 1024 characters")
        if self.instruction_pc is not None:
            _integer(self.instruction_pc, "instruction PC", 0, MASK64)


ForeignEventV1: TypeAlias = (
    ForeignCallbackRequestV1 | ForeignCompletedV1 | ForeignRunnableYieldV1 | ForeignFailedV1
)


@dataclass(frozen=True, slots=True, kw_only=True, eq=False)
class ForeignCancellationV1:
    """Retired suffix, deepest first; cancellation creates no new work receipt.

    A surviving parent retains its original pending callback token. The latest
    receipt may describe a discarded child, not that parent. An inactive cancel
    is empty and still exposes the last receipt for exception-path settlement.
    """

    retired_invocation_ids: tuple[int, ...]
    surviving_parent_id: int | None = None
    surviving_parent_token: object | None = field(default=None, repr=False)
    receipt: ForeignReceiptV1 | None = None
    abi: str = FOREIGN_ABI
    version: int = FOREIGN_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self)
        _tuple(self.retired_invocation_ids, "retired invocation IDs", MAX_DEPTH)
        for invocation in self.retired_invocation_ids:
            _integer(invocation, "retired invocation ID", 1, MASK64)
        if len(set(self.retired_invocation_ids)) != len(self.retired_invocation_ids):
            raise ValueError("retired invocation IDs must be distinct")
        if self.receipt is not None:
            _exact(self.receipt, ForeignReceiptV1, "receipt")
        elif self.retired_invocation_ids:
            raise ValueError("retiring an accepted invocation requires its retained receipt")
        if self.surviving_parent_id is None:
            if self.surviving_parent_token is not None:
                raise ValueError("surviving parent ID and token must appear together")
        else:
            _integer(self.surviving_parent_id, "surviving parent ID", 1, MASK64)
            _opaque(self.surviving_parent_token, "surviving parent token")
            if (not self.retired_invocation_ids
                    or self.surviving_parent_id in self.retired_invocation_ids):
                raise ValueError("surviving parent must remain outside a nonempty retired suffix")


class ForeignAdapterV1(Protocol):
    """Dispatcher-owned transitions, with opaque authority checked by identity.

    A child begin accepts only its live parent's pending callback authority.
    The adapter enforces original allowances as well as the supplied remaining
    ceilings; values cannot replenish fuel. begin preflight rejection publishes
    no receipt. Accepted zero-work segments do publish a fresh receipt.

    Each boundary retains last_receipt before event allocation; callers settle
    it even when a raw host exception escapes. No native execution lock spans
    a callback or host suspension. Cancellation is deepest-first and cannot
    execute a RET, produce outputs, replay a prefix or create a segment.
    """

    def begin(
        self, operation: ForeignOperationV1, arguments: tuple[int, ...], *,
        root_token: object, root_id: int, budget: ForeignBudgetV1,
        parent: ForeignCallbackRequestV1 | None = None,
    ) -> ForeignEventV1: ...

    def advance(self, operation_token: object, *, budget: ForeignBudgetV1) -> ForeignEventV1: ...

    def reply(
        self, request_token: object, outputs: tuple[int, ...], *, budget: ForeignBudgetV1,
    ) -> ForeignEventV1: ...

    def cancel_suffix(self, operation_token: object) -> ForeignCancellationV1: ...

    def cancel_all(self) -> ForeignCancellationV1: ...

    def last_receipt(self) -> ForeignReceiptV1 | None: ...


__all__ = [
    "FOREIGN_ABI", "FOREIGN_ABI_VERSION", "MAX_SIGNATURE_CELLS", "MAX_GRANTS",
    "MAX_DEPTH", "MAX_ROOT_ENTRIES", "MAX_CALLBACK_SITES",
    "MAX_INVOCATION_INSTRUCTIONS", "MAX_ROOT_INSTRUCTIONS", "MAX_CALLBACK_REQUESTS",
    "MAX_CALLBACK_SEMANTIC_STEPS", "MAX_ROOT_CALLBACK_SEMANTIC_STEPS", "MAX_DETAIL_CHARS",
    "ForeignAccessV1", "ForeignStateV1", "ForeignFailureKindV1", "ForeignSignatureV1",
    "ForeignSpanV1", "ForeignOperationV1", "ForeignExportV1", "ForeignBudgetV1",
    "ForeignReceiptV1", "ForeignCallbackRequestV1", "ForeignCompletedV1",
    "ForeignRunnableYieldV1", "ForeignFailedV1", "ForeignEventV1",
    "ForeignCancellationV1", "ForeignAdapterV1",
]
