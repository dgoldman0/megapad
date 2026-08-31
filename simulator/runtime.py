"""Explicit hosted MegaForth compiler and semantic dispatch loop.

The first runtime slice deliberately implements a small real kernel rather
than translating source into Python calls.  Colon continuations remain on the
same ordered return stack as user ``>R`` cells and counted-loop state, and
compiled calls retain the execution token that was bound at compile time.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, NoReturn, TypeAlias

from shared.cells import CELL_BYTES, MASK64, s64, u64
from shared.crypto_caps import (
    CRYPTO_CAP_CRC_REFLECT_RAW,
    CRYPTO_CAP_KECCAK_F1600,
    CRYPTO_CAP_SHA3_STREAM,
)
from simulator.crc import GuestIdentity, HostedCRCService
from simulator.diagnostics import HostedDiagnosticsService
from simulator.dictionary import Dictionary, Word
from simulator.dictionary_index import (
    DictionaryIndexState,
    HostedDictionaryIndex,
)
from simulator.errors import (
    ExecutionError,
    ForthAbort,
    SourceError,
    StepBudgetExceeded,
)
from simulator.field import HostedFieldALUService
from simulator.ir import (
    AbortIf,
    Branch,
    BranchZero,
    Call,
    Do,
    InstallDoes,
    Literal,
    Loop,
    Operation,
    QuestionDo,
    RestoreDataStackPointer,
    RestoreReturnStackPointer,
    RPeek,
    RPop,
    RPush,
    Return,
    Unloop,
    WriteOutput,
)
from simulator.kem import HostedKEMService
from simulator.memory import MMIO_BASE, AddressClass, SparseAddressSpace
from simulator.ntt import HostedNTTService
from simulator.platform import (
    HOSTED_CRYPTO_CAPABILITIES,
    OneCorePlatformMMIO,
    SYSINFO_CRYPTO_CAPS,
    SYSINFO_NUM_CORES,
    SYSINFO_NUM_FULL,
)
from simulator.sha2 import HostedSHA2Service
from simulator.spinlocks import HostedSpinlockBank
from simulator.source import (
    ASCII_SPACE,
    SourceBuffer,
    SourceCursor,
    SourceLocation,
)
from simulator.stacks import DataStack, ReturnStack


@dataclass(slots=True)
class ExecutionContext:
    """Per-task semantic stacks for one hosted execution context."""

    data: DataStack = field(default_factory=DataStack)
    returns: ReturnStack = field(default_factory=ReturnStack)
    _host_control_fault: str | None = field(default=None, init=False, repr=False)

    @property
    def reusable(self) -> bool:
        """Whether another public dispatch can safely use this context."""

        return self._host_control_fault is None

    @property
    def host_control_fault(self) -> str | None:
        """Explain why a host escape made this context non-reusable."""

        return self._host_control_fault

    def _require_reusable(self) -> None:
        if self._host_control_fault is not None:
            raise ExecutionError(
                "execution context is not reusable after a host escape across "
                f"an RP@-observing dispatch: {self._host_control_fault}"
            )

    def _mark_host_control_fault(self, error: BaseException) -> None:
        if self._host_control_fault is None:
            self._host_control_fault = type(error).__name__


@dataclass(frozen=True, slots=True)
class Invoke:
    """Primitive request to invoke a dynamic execution token."""

    xt: int

    def __post_init__(self) -> None:
        token = u64(self.xt)
        if token == 0:
            raise ExecutionError("execution token zero is not callable")
        object.__setattr__(self, "xt", token)


PrimitiveCallback: TypeAlias = Callable[[ExecutionContext], Invoke | None]


@dataclass(frozen=True, slots=True)
class PrimitiveDefinition:
    callback: PrimitiveCallback


@dataclass(frozen=True, slots=True)
class ColonDefinition:
    operations: tuple[Operation, ...]


@dataclass(frozen=True, slots=True)
class ConstantDefinition:
    value: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "value", u64(self.value))


@dataclass(frozen=True, slots=True)
class DoesBodyRef:
    """Stable entry point for one CREATE-family word's DOES> behavior."""

    source_xt: int
    entry_ip: int

    def __post_init__(self) -> None:
        source_xt = u64(self.source_xt)
        if source_xt == 0:
            raise ValueError("DOES> source execution token must be nonzero")
        if not isinstance(self.entry_ip, int):
            raise TypeError("DOES> entry instruction pointer must be an integer")
        if self.entry_ip < 0:
            raise ValueError("DOES> entry instruction pointer must not be negative")
        object.__setattr__(self, "source_xt", source_xt)


@dataclass(slots=True)
class CreatedDefinition:
    """Mutable runtime metadata for one CREATE-family child."""

    action: DoesBodyRef | None = None

    def __post_init__(self) -> None:
        if self.action is not None and not isinstance(self.action, DoesBodyRef):
            raise TypeError("created-word action must be a DoesBodyRef or None")


class DirectiveKind(Enum):
    COLON = auto()
    SEMICOLON = auto()
    IF = auto()
    ELSE = auto()
    THEN = auto()
    BEGIN = auto()
    UNTIL = auto()
    AGAIN = auto()
    WHILE = auto()
    REPEAT = auto()
    EXIT = auto()
    TO_R = auto()
    R_FROM = auto()
    R_FETCH = auto()
    DO = auto()
    QUESTION_DO = auto()
    LOOP = auto()
    LEAVE = auto()
    UNLOOP = auto()
    BRACKET_TICK = auto()
    DOES = auto()
    SP_STORE = auto()
    RP_STORE = auto()
    PAREN_COMMENT = auto()
    BACKSLASH_COMMENT = auto()
    PROVIDED = auto()
    DOT_QUOTE = auto()
    ABORT_QUOTE = auto()


@dataclass(frozen=True, slots=True)
class DirectiveDefinition:
    kind: DirectiveKind


WordImplementation: TypeAlias = (
    PrimitiveDefinition
    | ColonDefinition
    | ConstantDefinition
    | CreatedDefinition
    | DirectiveDefinition
)


@dataclass(frozen=True, slots=True)
class EvaluationResult:
    source_name: str
    line_count: int
    token_count: int
    semantic_steps: int
    definitions: tuple[Word, ...]


@dataclass(frozen=True, slots=True)
class ExecutionResult:
    semantic_steps: int


@dataclass(slots=True)
class _IfFrame:
    branch_index: int
    has_else: bool = False


@dataclass(slots=True)
class _DoFrame:
    body_target: int
    question_index: int | None = None
    leave_indices: list[int] = field(default_factory=list)


@dataclass(frozen=True, slots=True)
class _BeginFrame:
    body_target: int


@dataclass(frozen=True, slots=True)
class _WhileFrame:
    body_target: int
    branch_index: int


_ControlFrame: TypeAlias = _IfFrame | _DoFrame | _BeginFrame | _WhileFrame


@dataclass(slots=True)
class _Compiler:
    name: bytes
    location: SourceLocation
    operations: list[Operation] = field(default_factory=list)
    controls: list[_ControlFrame] = field(default_factory=list)


@dataclass(slots=True)
class _EvaluationState:
    context: ExecutionContext
    cursor: SourceCursor
    meter: _StepMeter
    compiler: _Compiler | None = None
    token_count: int = 0
    definitions: list[Word] = field(default_factory=list)


class _StepMeter:
    def __init__(
        self,
        budget: int | None,
        on_tick: Callable[[], None],
    ) -> None:
        if budget is not None:
            if not isinstance(budget, int):
                raise TypeError("step budget must be an integer or None")
            if budget < 1:
                raise ValueError("step budget must be positive")
        self.budget = budget
        self.steps = 0
        self._on_tick = on_tick

    def tick(self) -> None:
        if self.budget is not None and self.steps >= self.budget:
            raise StepBudgetExceeded(self.budget)
        self.steps += 1
        self._on_tick()


@dataclass(frozen=True, slots=True)
class _DispatchFrame:
    """One nested host boundary participating in semantic dispatch."""

    context: ExecutionContext
    meter: _StepMeter
    root_id: int


class _DictionaryFaultRequest(BaseException):
    """Nonlocal request to enter the installed guest fault callback."""

    def __init__(
        self,
        xt: int,
        context: ExecutionContext,
        reason: str,
    ) -> None:
        self.xt = u64(xt)
        self.context = context
        self.reason = reason
        super().__init__(reason)


class _GuestControlTransfer(BaseException):
    """A nested dispatcher consumed an older public dispatch root."""

    def __init__(self, root_id: int, context: ExecutionContext) -> None:
        self.root_id = u64(root_id)
        self.context = context
        super().__init__(f"guest control transferred to dispatch {self.root_id}")


class MegaForthRuntime:
    """Hosted dictionary, evaluator, and explicit semantic dispatcher."""

    def __init__(
        self,
        *,
        dictionary_start: int = 0x1000,
        memory: SparseAddressSpace | None = None,
        diagnostics: HostedDiagnosticsService | None = None,
        install_core_words: bool = True,
    ) -> None:
        if memory is not None and not isinstance(memory, SparseAddressSpace):
            raise TypeError("memory must be a SparseAddressSpace or None")
        if diagnostics is not None and not isinstance(
            diagnostics,
            HostedDiagnosticsService,
        ):
            raise TypeError(
                "diagnostics must be a HostedDiagnosticsService or None"
            )
        if memory is None:
            from simulator.platform import create_one_core_address_space

            self.memory = create_one_core_address_space()
        else:
            self.memory = memory
        num_cores = self.memory.read64(MMIO_BASE + SYSINFO_NUM_CORES)
        num_full = self.memory.read64(MMIO_BASE + SYSINFO_NUM_FULL)
        if (num_cores, num_full) != (1, 1):
            raise ValueError(
                "hosted runtime requires exactly one advertised full core"
            )
        crypto_capabilities = self.memory.read64(
            MMIO_BASE + SYSINFO_CRYPTO_CAPS
        )
        if crypto_capabilities & ~HOSTED_CRYPTO_CAPABILITIES:
            raise ValueError(
                "hosted runtime cannot admit unknown crypto capabilities"
            )
        platform_mmio = self.memory.mmio
        if not isinstance(platform_mmio, OneCorePlatformMMIO):
            raise ValueError(
                "hosted runtime requires the admitted one-core platform MMIO"
            )
        crc_capabilities = (
            crypto_capabilities & CRYPTO_CAP_CRC_REFLECT_RAW
        )
        sha_capabilities = crypto_capabilities & (
            CRYPTO_CAP_SHA3_STREAM | CRYPTO_CAP_KECCAK_F1600
        )
        if platform_mmio.sha3.capabilities != sha_capabilities:
            raise ValueError(
                "SysInfo and hosted SHA capabilities do not agree"
            )
        self.crc = HostedCRCService(crc_capabilities)
        self.aes = platform_mmio.aes
        self.sha3 = platform_mmio.sha3
        self.entropy = platform_mmio.entropy
        self.sha2 = HostedSHA2Service(core_count=num_full)
        self.spinlocks = HostedSpinlockBank(core_count=num_cores)
        self.field = HostedFieldALUService(core_count=num_cores)
        self.ntt = HostedNTTService()
        self.kem = HostedKEMService()
        self.diagnostics = (
            HostedDiagnosticsService()
            if diagnostics is None
            else diagnostics.clone()
        )
        self.dictionary = Dictionary(
            start_address=dictionary_start,
            memory=self.memory,
        )
        self.dictionary_index = HostedDictionaryIndex(
            self.memory,
            self.dictionary,
        )
        bank0 = next(
            (
                region
                for region in self.memory.regions
                if region.kind is AddressClass.BANK0
            ),
            None,
        )
        if bank0 is None:
            raise ValueError("hosted runtime requires a mapped Bank 0")
        self._bank0 = bank0
        data_empty = bank0.base + bank0.size // 2
        return_empty = bank0.limit
        if (
            data_empty <= bank0.base
            or return_empty <= data_empty
            or data_empty % CELL_BYTES
            or return_empty % CELL_BYTES
        ):
            raise ValueError(
                "Bank 0 must provide cell-aligned data and return stack halves"
            )
        self.main_context = ExecutionContext(
            data=DataStack(
                memory=self.memory,
                floor=bank0.base,
                empty_pointer=data_empty,
            ),
            returns=ReturnStack(
                memory=self.memory,
                floor=data_empty,
                empty_pointer=return_empty,
            ),
        )
        self._bootstrap_numeric_base = 10
        self._numeric_base_address: int | None = None
        # These are the source-visible optional user-dictionary bounds, not
        # Dictionary's ordinary containing-memory ceiling.  Bank-0 source
        # starts with the BIOS interval disabled, represented by two zeros.
        self._dictionary_base = 0
        self._dictionary_limit = 0
        self._dictionary_fault_xt = 0
        self._provided: set[bytes] = set()
        self._active_input_states: list[_EvaluationState] = []
        self._active_dispatches: list[_DispatchFrame] = []
        self._next_dispatch_root_id = 1
        self._uart_output = bytearray()
        if install_core_words:
            from simulator.core_words import install_core

            install_core(self)
            self.dictionary.protect_current_prefix_from_numeric_rollback()

    @property
    def provided_modules(self) -> frozenset[bytes]:
        return frozenset(self._provided)

    def revoke_provided_module(self, module_id: bytes) -> None:
        """Discard one provisional bootstrap publication after load failure.

        This narrow host seam exists because source commonly executes
        ``PROVIDED`` before a later dependency.  The pre-KDOS loader must be
        able to roll back only the failed outer publication while retaining
        dependencies that completed successfully.  KDOS later shadows both
        source words with its ordinary registry implementation.
        """

        if not isinstance(module_id, bytes):
            raise TypeError("module ID must be bytes")
        if not module_id:
            raise ValueError("module ID must not be empty")
        self._provided.discard(module_id)

    @property
    def numeric_base(self) -> int:
        """Current unsigned cell used by the BIOS-compatible number parser."""

        if self._numeric_base_address is None:
            return self._bootstrap_numeric_base
        return self.memory.read64(self._numeric_base_address)

    @property
    def dictionary_base(self) -> int:
        """Return the active BIOS user-dictionary base, or zero when off."""

        return self._dictionary_base

    @property
    def dictionary_limit(self) -> int:
        """Return the active BIOS user-dictionary limit, or zero when off."""

        return self._dictionary_limit

    @property
    def dictionary_fault_xt(self) -> int:
        """Return the raw callback installed through ``DICT-FAULT-XT!``."""

        return self._dictionary_fault_xt

    @property
    def dictionary_index_state(self) -> DictionaryIndexState:
        """Return stable diagnostics for the caller-backed BIOS index."""

        return self.dictionary_index.state

    @property
    def uart_output(self) -> bytes:
        """Return an immutable snapshot of bytes written to the hosted UART."""

        return bytes(self._uart_output)

    def write_uart_bytes(self, payload: bytes) -> None:
        """Append one validated byte string to the hosted UART stream."""

        if not isinstance(payload, bytes):
            raise TypeError("UART output payload must be bytes")
        self._uart_output.extend(payload)

    def drain_uart_output(self) -> bytes:
        """Return all pending UART bytes and clear the runtime-owned buffer."""

        payload = bytes(self._uart_output)
        self._uart_output.clear()
        return payload

    def set_numeric_base(self, base: int) -> None:
        """Set the source number base as a wrapped guest cell.

        The BIOS exposes ``BASE`` as a mutable cell and does not constrain its
        contents.  A core-less runtime retains a bootstrap value; the ordinary
        semantic BIOS binds this method to the live guest cell.
        """

        value = u64(base)
        if self._numeric_base_address is None:
            self._bootstrap_numeric_base = value
        else:
            self.memory.write64(self._numeric_base_address, value)

    def bind_numeric_base_address(self, address: int) -> None:
        """Bind numeric parsing and printers to the semantic BIOS BASE cell."""

        if self._numeric_base_address is not None:
            raise RuntimeError("numeric BASE is already bound")
        self.memory.write64(address, self._bootstrap_numeric_base)
        self._numeric_base_address = address

    def guest_identity(self, context: ExecutionContext) -> GuestIdentity:
        """Return the checked BIOS caller identity for the initial profile."""

        if not isinstance(context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        # Host scratch contexts are not schedulable guest tasks.  Until the
        # deterministic task layer exists, every dispatch is core 0, task 0.
        return 0, 0

    def caller_span_status(
        self,
        context: ExecutionContext,
        address: int,
        length: int,
    ) -> int:
        """Qualify one complete caller-managed ordinary-memory span.

        Results are the protocol-neutral BIOS values: ``0`` for admitted,
        ``2`` for malformed/unmapped/cross-region ranges, and ``3`` for the
        protected Bank-0 prefix or live stack boundary.  As in BIOS, an empty
        span ignores its otherwise-unused address.
        """

        if not isinstance(context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        address = u64(address)
        length = u64(length)
        if length == 0:
            return 0
        if length & (1 << 63) or address == 0 or address & (1 << 63):
            return 2

        end = address + length
        if end > (1 << 64):
            return 2
        region = next(
            (
                candidate
                for candidate in self.memory.regions
                if candidate.base <= address and end <= candidate.limit
            ),
            None,
        )
        if region is None:
            return 2
        if region.kind is not AddressClass.BANK0:
            return 0

        if address < self.dictionary.numeric_rollback_floor:
            return 3
        stack = context.data if context.data.backed else self.main_context.data
        result_boundary = stack.pointer - CELL_BYTES
        if end > result_boundary:
            return 3
        return 0

    def set_dictionary_fault_xt(self, xt: int) -> None:
        """Install a raw guest callback, including zero to disable it."""

        self._dictionary_fault_xt = u64(xt)

    def configure_dictionary_index(self, base: int, slots: int) -> int:
        """Install, rebuild, or disable the caller-backed BIOS index."""

        return self.dictionary_index.configure(base, slots)

    def _dictionary_context(
        self,
        context: ExecutionContext | None = None,
    ) -> ExecutionContext:
        if context is not None:
            return context
        if self._active_dispatches:
            return self._active_dispatches[-1].context
        if self._active_input_states:
            return self._active_input_states[-1].context
        return self.main_context

    def _has_active_dispatch(self, context: ExecutionContext) -> bool:
        return any(
            frame.context is context for frame in self._active_dispatches
        )

    def _has_active_evaluation(self, context: ExecutionContext) -> bool:
        return any(
            state.context is context for state in self._active_input_states
        )

    def _has_older_dispatch(self, context: ExecutionContext) -> bool:
        if not self._active_dispatches:
            return False
        current = self._active_dispatches[-1]
        if current.context is not context:
            raise AssertionError("semantic dispatch context stack is corrupted")
        return any(
            frame.context is context for frame in self._active_dispatches[:-1]
        )

    def _allocate_dispatch_root_id(self) -> int:
        root_id = self._next_dispatch_root_id
        self._next_dispatch_root_id += 1
        if root_id > MASK64:
            raise ExecutionError("semantic dispatch identity space exhausted")
        return root_id

    def _request_dictionary_fault(
        self,
        context: ExecutionContext,
        reason: str,
    ) -> NoReturn:
        raise _DictionaryFaultRequest(
            self._dictionary_fault_xt,
            context,
            reason,
        )

    def _preflight_dictionary_growth(
        self,
        width: int,
        context: ExecutionContext,
    ) -> None:
        if not isinstance(width, int) or width < 0:
            raise TypeError("dictionary growth width must be a nonnegative integer")

        start = self.dictionary.here
        if start > MASK64 - width:
            self._request_dictionary_fault(context, "dictionary growth wraps uint64")
        target = start + width
        if self._dictionary_limit:
            if (
                start < self._dictionary_base
                or start > self._dictionary_limit
                or target > self._dictionary_limit
            ):
                self._request_dictionary_fault(
                    context,
                    "dictionary growth exceeds the active user interval",
                )
            return

        stack = context.data if context.data.backed else self.main_context.data
        ceiling = stack.pointer - 256
        if (
            start < self._bank0.base
            or start >= self._bank0.limit
            or target > self._bank0.limit
            or target > ceiling
        ):
            self._request_dictionary_fault(
                context,
                "dictionary growth exceeds the guarded Bank-0 interval",
            )

    def _preflight_dictionary_target(
        self,
        target: int,
        context: ExecutionContext,
    ) -> None:
        if not 0 <= target <= MASK64:
            self._request_dictionary_fault(context, "dictionary target wraps uint64")
        if self._dictionary_limit:
            if not self._dictionary_base <= target <= self._dictionary_limit:
                self._request_dictionary_fault(
                    context,
                    "dictionary target is outside the active user interval",
                )
            return

        stack = context.data if context.data.backed else self.main_context.data
        ceiling = stack.pointer - 256
        if not self._bank0.base <= target < self._bank0.limit or target > ceiling:
            self._request_dictionary_fault(
                context,
                "dictionary target is outside the guarded Bank-0 interval",
            )

    def _define_dictionary_word(
        self,
        name: bytes | str,
        implementation: WordImplementation,
        *,
        immediate: bool = False,
        initial_body: bytes = b"",
        context: ExecutionContext | None = None,
    ) -> Word:
        active_context = self._dictionary_context(context)
        width = self.dictionary.definition_size(name, initial_body=initial_body)
        self._preflight_dictionary_growth(width, active_context)
        try:
            word = self.dictionary.define(
                name,
                implementation,
                immediate=immediate,
                initial_body=initial_body,
            )
        except OverflowError as exc:
            self._request_dictionary_fault(active_context, str(exc))
        self.dictionary_index.publish(word)
        return word

    def _route_unhandled_dictionary_fault(
        self,
        request: _DictionaryFaultRequest,
    ) -> NoReturn:
        """Give direct host definition calls the same fail-closed boundary."""

        if self._has_active_dispatch(request.context) or any(
            state.context is request.context
            for state in self._active_input_states
        ):
            raise request
        self._execute_dictionary_fault_guarded(
            request,
            request.context,
            _StepMeter(None, self.diagnostics.account_work),
        )
        raise AssertionError("dictionary fault callback returned to its caller")

    def _define_public_dictionary_word(
        self,
        name: bytes | str,
        implementation: WordImplementation,
        *,
        immediate: bool = False,
        initial_body: bytes = b"",
    ) -> Word:
        try:
            return self._define_dictionary_word(
                name,
                implementation,
                immediate=immediate,
                initial_body=initial_body,
            )
        except _DictionaryFaultRequest as request:
            self._route_unhandled_dictionary_fault(request)

    def allot_dictionary(
        self,
        delta_cell: int,
        context: ExecutionContext,
    ) -> None:
        if not isinstance(delta_cell, int):
            self.dictionary.allot(delta_cell)
            return
        target = self.dictionary.here + s64(delta_cell)
        self._preflight_dictionary_target(target, context)
        try:
            self.dictionary.allot(delta_cell)
        except OverflowError as exc:
            self._request_dictionary_fault(context, str(exc))

    def comma_dictionary(self, cell: int, context: ExecutionContext) -> None:
        self._preflight_dictionary_growth(CELL_BYTES, context)
        try:
            self.dictionary.comma(cell)
        except OverflowError as exc:
            self._request_dictionary_fault(context, str(exc))

    def c_comma_dictionary(self, cell: int, context: ExecutionContext) -> None:
        self._preflight_dictionary_growth(1, context)
        try:
            self.dictionary.c_comma(cell)
        except OverflowError as exc:
            self._request_dictionary_fault(context, str(exc))

    def tile_align_dictionary(self, context: ExecutionContext) -> None:
        """Apply BIOS ``TALIGN`` growth semantics to the hosted frontier."""

        width = (-self.dictionary.here) & 63
        if width == 0:
            return
        self._preflight_dictionary_growth(width, context)
        try:
            self.dictionary.allot(width)
        except OverflowError as exc:
            self._request_dictionary_fault(context, str(exc))

    def rollback_dictionary(
        self,
        saved_here: int,
        saved_latest: int,
        context: ExecutionContext,
    ) -> None:
        self._preflight_dictionary_target(saved_here, context)
        try:
            self.dictionary.rollback_to(saved_here, saved_latest)
        except (TypeError, ValueError, RuntimeError) as exc:
            self._request_dictionary_fault(context, str(exc))
        self.dictionary_index.rebuild()

    def new_context(self) -> ExecutionContext:
        """Return an unbacked host scratch context.

        The canonical one-core guest stack geometry belongs to
        :attr:`main_context`.  Additional guest tasks will require explicit,
        caller-owned stack arenas rather than silently aliasing that storage.
        """

        return ExecutionContext()

    def find(self, name: bytes | str) -> Word | None:
        return self.dictionary.find(name)

    def define_primitive(
        self,
        name: bytes | str,
        callback: PrimitiveCallback,
        *,
        immediate: bool = False,
    ) -> Word:
        if not callable(callback):
            raise TypeError("primitive callback must be callable")
        return self._define_public_dictionary_word(
            name,
            PrimitiveDefinition(callback),
            immediate=immediate,
        )

    def define_constant(self, name: bytes | str, value: int) -> Word:
        """Publish an executable cell constant under one stable XT.

        Constants created by a defining primitive during :meth:`evaluate` are
        also included in that evaluation's ordered definition ledger.
        """

        word = self._define_public_dictionary_word(
            name,
            ConstantDefinition(value),
        )
        if self._active_input_states:
            self._active_input_states[-1].definitions.append(word)
        return word

    def define_created(
        self,
        name: bytes | str,
        *,
        initial_body: bytes = b"",
    ) -> Word:
        """Atomically publish one CREATE-family child and initial body bytes."""

        word = self._define_public_dictionary_word(
            name,
            CreatedDefinition(),
            initial_body=initial_body,
        )
        if self._active_input_states:
            self._active_input_states[-1].definitions.append(word)
        return word

    def define_directive(self, name: bytes | str, kind: DirectiveKind) -> Word:
        if not isinstance(kind, DirectiveKind):
            raise TypeError("directive kind must be a DirectiveKind")
        return self._define_public_dictionary_word(
            name,
            DirectiveDefinition(kind),
            immediate=True,
        )

    def parse_input_word(self) -> bytes:
        """Parse an optional word from the active physical input line."""

        if not self._active_input_states:
            raise ExecutionError("cannot parse a word without an active input line")
        return self._active_input_states[-1].cursor.parse_word()

    def parse_word_to_dictionary_tail(self, delimiter: int) -> int:
        """Publish one BIOS ``WORD`` value and then commit its input cursor."""

        if not self._active_input_states:
            raise ExecutionError(
                "cannot parse a delimited word without an active input line"
            )
        cursor = self._active_input_states[-1].cursor
        value, next_column = cursor.preview_delimited_word(delimiter)
        counted = bytes((len(value.data) & 0xFF,)) + value.data + b"\0"
        active_context = self._dictionary_context()
        self._preflight_dictionary_growth(len(counted), active_context)
        try:
            address = self.dictionary.write_transient(counted)
        except OverflowError as exc:
            self._request_dictionary_fault(active_context, str(exc))
        committed = cursor.parse_delimited_word(delimiter)
        if committed != value or cursor.column != next_column:
            raise AssertionError("WORD cursor preview diverged before commit")
        return address

    def parse_required_input_word(self, owner: bytes | str) -> bytes:
        """Consume a required word from the active physical input line.

        This is the source-cursor boundary used by ordinary defining
        primitives.  It deliberately cannot continue onto a later line and
        is unavailable to execution that was not reached from
        :meth:`evaluate`.
        """

        if isinstance(owner, bytes):
            try:
                owner_text = owner.decode("ascii")
            except UnicodeDecodeError as exc:
                raise ValueError("input parser owner must be ASCII") from exc
        elif isinstance(owner, str):
            owner_text = owner
        else:
            raise TypeError("input parser owner must be bytes or str")
        if not owner_text:
            raise ValueError("input parser owner must not be empty")
        value = self.parse_input_word()
        if not value:
            state = self._active_input_states[-1]
            raise SourceError(
                f"{owner_text} requires a following word",
                state.cursor.location,
            )
        return value

    def evaluate(
        self,
        source: bytes | bytearray | memoryview,
        *,
        source_name: str = "<input>",
        context: ExecutionContext | None = None,
        step_budget: int | None = None,
    ) -> EvaluationResult:
        """Interpret and compile one complete byte source buffer.

        ``step_budget`` is shared by every word executed while consuming the
        buffer.  It intentionally does not reset at token boundaries.
        """

        active_context = self.main_context if context is None else context
        if not isinstance(active_context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        active_context._require_reusable()

        meter, starting_steps = self._meter_for_public_call(step_budget)
        capture_checkpoint = (
            active_context.returns.pointer_capture_checkpoint()
        )
        has_enclosing_dispatch = self._has_active_dispatch(active_context)
        state: _EvaluationState | None = None
        line_count = 0
        try:
            for line in SourceBuffer(source, name=source_name):
                line_count += 1
                cursor = line.cursor()
                if state is None:
                    state = _EvaluationState(active_context, cursor, meter)
                else:
                    state.cursor = cursor
                self._evaluate_line(state)

            if state is None:
                return EvaluationResult(source_name, 0, 0, 0, ())
            if state.compiler is not None:
                raise SourceError(
                    f"definition {state.compiler.name!r} has no terminating ;",
                    state.compiler.location,
                )
            return EvaluationResult(
                source_name=source_name,
                line_count=line_count,
                token_count=state.token_count,
                semantic_steps=meter.steps - starting_steps,
                definitions=tuple(state.definitions),
            )
        except _DictionaryFaultRequest as request:
            # Nested evaluation must hand the request back to the suspended
            # semantic dispatcher so guest THROW can discard its fault
            # sentinel and resume the existing CATCH continuation.
            if request.context is not active_context:
                if active_context.returns.has_pointer_captures_after(
                    capture_checkpoint
                ):
                    active_context._mark_host_control_fault(request)
                raise
            if self._has_active_dispatch(active_context):
                raise
            try:
                self._execute_dictionary_fault_guarded(
                    request,
                    active_context,
                    meter,
                )
            except _GuestControlTransfer as transfer:
                if (
                    transfer.context is not active_context
                    and active_context.returns.has_pointer_captures_after(
                        capture_checkpoint
                    )
                ):
                    active_context._mark_host_control_fault(transfer)
                raise
            except ForthAbort as exc:
                if active_context.returns.has_pointer_captures_after(
                    capture_checkpoint
                ):
                    active_context._mark_host_control_fault(exc)
                if exc.bind_origin(active_context):
                    active_context.data.clear()
                    active_context.returns.clear()
                raise
            except BaseException as exc:
                if active_context.returns.has_pointer_captures_after(
                    capture_checkpoint
                ):
                    active_context._mark_host_control_fault(exc)
                raise
            raise AssertionError("dictionary fault callback returned to evaluator")
        except _GuestControlTransfer as transfer:
            if (
                transfer.context is not active_context
                and active_context.returns.has_pointer_captures_after(
                    capture_checkpoint
                )
            ):
                active_context._mark_host_control_fault(transfer)
            raise
        except ForthAbort as exc:
            if active_context.returns.has_pointer_captures_after(
                capture_checkpoint
            ):
                active_context._mark_host_control_fault(exc)
            if exc.bind_origin(active_context):
                active_context.data.clear()
                active_context.returns.clear()
            raise
        except BaseException as exc:
            if active_context.returns.has_pointer_captures_after(
                capture_checkpoint
            ):
                active_context._mark_host_control_fault(exc)
            raise
        finally:
            if not has_enclosing_dispatch:
                active_context.returns.restore_pointer_captures(
                    capture_checkpoint
                )

    def execute(
        self,
        name_or_xt: bytes | str | int,
        *,
        context: ExecutionContext | None = None,
        step_budget: int | None = None,
    ) -> ExecutionResult:
        """Execute one live word in a context using explicit dispatch."""

        active_context = self.main_context if context is None else context
        if not isinstance(active_context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        active_context._require_reusable()
        word = self._resolve_word(name_or_xt)
        meter, starting_steps = self._meter_for_public_call(step_budget)
        self._execute_guarded(word, active_context, meter)
        return ExecutionResult(meter.steps - starting_steps)

    def _evaluate_line(self, state: _EvaluationState) -> None:
        self._active_input_states.append(state)
        try:
            while True:
                token = state.cursor.parse_word()
                if not token:
                    return
                state.token_count += 1
                self._evaluate_token(token, state)
        finally:
            active = self._active_input_states.pop()
            if active is not state:
                raise AssertionError("active input cursor stack is corrupted")

    def _evaluate_token(self, token: bytes, state: _EvaluationState) -> None:
        if token.startswith(b"\\"):
            state.cursor.skip_backslash_comment()
            return

        word = self.dictionary.find(token)
        if word is not None and isinstance(word.implementation, DirectiveDefinition):
            self._apply_directive(word.implementation.kind, state)
            return

        if word is not None:
            if state.compiler is not None and not word.immediate:
                state.compiler.operations.append(Call(word.xt))
            else:
                self._execute_guarded(word, state.context, state.meter)
            return

        number = self._parse_number(token)
        if number is not None:
            if state.compiler is None:
                state.context.data.push(number)
            else:
                state.compiler.operations.append(Literal(number))
            return

        raise SourceError(
            f"unknown word {token!r}",
            self._token_location(state),
        )

    def _apply_directive(
        self,
        kind: DirectiveKind,
        state: _EvaluationState,
    ) -> None:
        if kind is DirectiveKind.PAREN_COMMENT:
            state.cursor.skip_parenthesis_comment()
            return
        if kind is DirectiveKind.BACKSLASH_COMMENT:
            state.cursor.skip_backslash_comment()
            return
        if kind is DirectiveKind.PROVIDED:
            self._provided.add(self._parse_required_word(state, "PROVIDED"))
            return
        if kind is DirectiveKind.DOT_QUOTE:
            payload = self._parse_quoted_literal(
                state,
                unconditionally_skip_next=True,
            )
            if state.compiler is None:
                self.write_uart_bytes(payload)
            else:
                state.compiler.operations.append(WriteOutput(payload))
            return
        if kind is DirectiveKind.ABORT_QUOTE:
            if state.compiler is None:
                self._compile_error(state, 'ABORT" is compile-only')
            payload = self._parse_quoted_literal(
                state,
                unconditionally_skip_next=False,
            )
            assert state.compiler is not None
            state.compiler.operations.append(AbortIf(payload))
            return
        if kind is DirectiveKind.COLON:
            if state.compiler is not None:
                self._compile_error(state, "nested : is not supported")
            name = self._parse_required_word(state, ":")
            # Native ':' proves its header span immediately after consuming
            # the name.  Hosted IR remains buffered until ';', but the early
            # preflight prevents later immediate words from running when the
            # definition could not have been opened by BIOS.
            self._preflight_dictionary_growth(
                self.dictionary.definition_size(name),
                state.context,
            )
            state.compiler = _Compiler(name, self._token_location(state))
            return

        compiler = state.compiler
        if compiler is None:
            self._compile_error(state, f"{kind.name} is compile-only")

        if kind is DirectiveKind.SEMICOLON:
            if compiler.controls:
                self._compile_error(state, "; has unresolved control flow")
            operations = tuple((*compiler.operations, Return()))
            word = self._define_dictionary_word(
                compiler.name,
                ColonDefinition(operations),
                context=state.context,
            )
            state.definitions.append(word)
            state.compiler = None
        elif kind is DirectiveKind.IF:
            compiler.operations.append(BranchZero(0))
            compiler.controls.append(_IfFrame(len(compiler.operations) - 1))
        elif kind is DirectiveKind.ELSE:
            frame = self._require_if_frame(state, allow_else=False)
            compiler.operations.append(Branch(0))
            else_index = len(compiler.operations) - 1
            compiler.operations[frame.branch_index] = BranchZero(else_index + 1)
            frame.branch_index = else_index
            frame.has_else = True
        elif kind is DirectiveKind.THEN:
            frame = self._require_if_frame(state, allow_else=True)
            target = len(compiler.operations)
            compiler.operations[frame.branch_index] = (
                Branch(target) if frame.has_else else BranchZero(target)
            )
            compiler.controls.pop()
        elif kind is DirectiveKind.BEGIN:
            compiler.controls.append(_BeginFrame(len(compiler.operations)))
        elif kind is DirectiveKind.UNTIL:
            frame = self._require_begin_frame(state, owner="UNTIL")
            compiler.controls.pop()
            compiler.operations.append(BranchZero(frame.body_target))
        elif kind is DirectiveKind.AGAIN:
            frame = self._require_begin_frame(state, owner="AGAIN")
            compiler.controls.pop()
            compiler.operations.append(Branch(frame.body_target))
        elif kind is DirectiveKind.WHILE:
            frame = self._require_begin_frame(state, owner="WHILE")
            compiler.controls.pop()
            compiler.operations.append(BranchZero(0))
            compiler.controls.append(
                _WhileFrame(
                    body_target=frame.body_target,
                    branch_index=len(compiler.operations) - 1,
                )
            )
        elif kind is DirectiveKind.REPEAT:
            frame = self._require_while_frame(state)
            compiler.controls.pop()
            compiler.operations.append(Branch(frame.body_target))
            compiler.operations[frame.branch_index] = BranchZero(
                len(compiler.operations)
            )
        elif kind is DirectiveKind.EXIT:
            compiler.operations.append(Return())
        elif kind is DirectiveKind.TO_R:
            compiler.operations.append(RPush())
        elif kind is DirectiveKind.R_FROM:
            compiler.operations.append(RPop())
        elif kind is DirectiveKind.R_FETCH:
            compiler.operations.append(RPeek())
        elif kind is DirectiveKind.DO:
            compiler.operations.append(Do())
            compiler.controls.append(_DoFrame(len(compiler.operations)))
        elif kind is DirectiveKind.QUESTION_DO:
            compiler.operations.append(QuestionDo(0))
            question_index = len(compiler.operations) - 1
            compiler.controls.append(
                _DoFrame(len(compiler.operations), question_index)
            )
        elif kind is DirectiveKind.LOOP:
            if not compiler.controls or not isinstance(
                compiler.controls[-1], _DoFrame
            ):
                self._compile_error(state, "LOOP has no matching DO")
            frame = compiler.controls.pop()
            assert isinstance(frame, _DoFrame)
            compiler.operations.append(Loop(frame.body_target))
            exit_target = len(compiler.operations)
            if frame.question_index is not None:
                compiler.operations[frame.question_index] = QuestionDo(
                    exit_target
                )
            for leave_index in frame.leave_indices:
                compiler.operations[leave_index] = Branch(exit_target)
        elif kind is DirectiveKind.LEAVE:
            frame = next(
                (
                    control
                    for control in reversed(compiler.controls)
                    if isinstance(control, _DoFrame)
                ),
                None,
            )
            if frame is None:
                self._compile_error(state, "LEAVE has no matching DO")
            assert isinstance(frame, _DoFrame)
            compiler.operations.append(Unloop())
            compiler.operations.append(Branch(0))
            frame.leave_indices.append(len(compiler.operations) - 1)
        elif kind is DirectiveKind.UNLOOP:
            compiler.operations.append(Unloop())
        elif kind is DirectiveKind.BRACKET_TICK:
            name = self.parse_input_word()
            word = self.dictionary.find(name) if name else None
            compiler.operations.append(Literal(0 if word is None else word.xt))
        elif kind is DirectiveKind.DOES:
            entry_ip = len(compiler.operations) + 2
            compiler.operations.append(InstallDoes(entry_ip))
            compiler.operations.append(Return())
        elif kind is DirectiveKind.SP_STORE:
            compiler.operations.append(RestoreDataStackPointer())
        elif kind is DirectiveKind.RP_STORE:
            compiler.operations.append(RestoreReturnStackPointer())
        else:
            raise AssertionError(f"unhandled directive {kind}")

    def _execute_guarded(
        self,
        word: Word,
        context: ExecutionContext,
        meter: _StepMeter,
    ) -> None:
        """Execute atomically with respect to internal return-stack state."""

        context._require_reusable()
        return_snapshot = context.returns.snapshot()
        capture_checkpoint = context.returns.pointer_capture_checkpoint()
        root_id = self._allocate_dispatch_root_id()
        frame = _DispatchFrame(context, meter, root_id)
        preserve_capture_evidence = False
        completed_successfully = False
        self._active_dispatches.append(frame)
        try:
            self._execute_top(word, context, meter, root_id=root_id)
            completed_successfully = True
        except _GuestControlTransfer as transfer:
            if transfer.context is not context:
                if context.returns.has_pointer_captures_after(
                    capture_checkpoint
                ):
                    context._mark_host_control_fault(transfer)
                context.returns.restore(return_snapshot)
                raise
            if transfer.root_id != root_id:
                preserve_capture_evidence = True
                raise
            # A nested public call executed through this frame's exact guest
            # root after RP! discarded its own Python dispatch boundary.  The
            # nested loop has already completed this semantic dispatch.
            completed_successfully = True
        except _DictionaryFaultRequest as request:
            # A nested public execute/evaluate boundary must not install a
            # fresh fault continuation above an older guest CATCH.  Remove
            # only this nested dispatch's internal return state and let the
            # request reach the outer semantic loop that owns that CATCH.
            capture_generation = context.returns.pointer_capture_checkpoint()
            if request.context is not context:
                if context.returns.has_pointer_captures_after(
                    capture_checkpoint
                ):
                    context._mark_host_control_fault(request)
                context.returns.restore(return_snapshot)
                raise
            context.returns.restore(return_snapshot)
            context.returns.restore_pointer_captures(capture_generation)
            preserve_capture_evidence = True
            raise
        except ForthAbort as exc:
            if context.returns.has_pointer_captures_after(capture_checkpoint):
                context._mark_host_control_fault(exc)
            if exc.bind_origin(context):
                # Normalize a direct host primitive ForthAbort to the same
                # complete-task reset as the BIOS word.
                context.data.clear()
                context.returns.clear()
            else:
                context.returns.restore(return_snapshot)
            raise
        except BaseException as exc:
            if context.returns.has_pointer_captures_after(capture_checkpoint):
                context._mark_host_control_fault(exc)
            context.returns.restore(return_snapshot)
            raise
        finally:
            if completed_successfully:
                if self._has_older_dispatch(
                    context
                ) or self._has_active_evaluation(context):
                    preserve_capture_evidence = True
            if not preserve_capture_evidence:
                context.returns.restore_pointer_captures(capture_checkpoint)
            active = self._active_dispatches.pop()
            if active is not frame:
                raise AssertionError("active semantic dispatch stack is corrupted")

    def _execute_dictionary_fault_guarded(
        self,
        request: _DictionaryFaultRequest,
        context: ExecutionContext,
        meter: _StepMeter,
    ) -> None:
        """Enter a top-level fault callback with ordinary host escape guards."""

        context._require_reusable()
        return_snapshot = context.returns.snapshot()
        capture_checkpoint = context.returns.pointer_capture_checkpoint()
        root_id = self._allocate_dispatch_root_id()
        frame = _DispatchFrame(context, meter, root_id)
        preserve_capture_evidence = False
        self._active_dispatches.append(frame)
        try:
            self._execute_top(
                None,
                context,
                meter,
                root_id=root_id,
                fault_request=request,
            )
        except _GuestControlTransfer as transfer:
            if transfer.context is context:
                preserve_capture_evidence = True
            else:
                if context.returns.has_pointer_captures_after(
                    capture_checkpoint
                ):
                    context._mark_host_control_fault(transfer)
                context.returns.restore(return_snapshot)
            raise
        except ForthAbort as exc:
            if context.returns.has_pointer_captures_after(capture_checkpoint):
                context._mark_host_control_fault(exc)
            if exc.bind_origin(context):
                context.data.clear()
                context.returns.clear()
            else:
                context.returns.restore(return_snapshot)
            raise
        except BaseException as exc:
            if context.returns.has_pointer_captures_after(capture_checkpoint):
                context._mark_host_control_fault(exc)
            context.returns.restore(return_snapshot)
            raise
        finally:
            if not preserve_capture_evidence:
                context.returns.restore_pointer_captures(capture_checkpoint)
            active = self._active_dispatches.pop()
            if active is not frame:
                raise AssertionError("active semantic dispatch stack is corrupted")

    def _abort_returned_dictionary_fault(
        self,
        context: ExecutionContext,
    ) -> None:
        self.write_uart_bytes(b"dictionary overflow\r\n")
        context.data.clear()
        context.returns.clear()
        raise ForthAbort(
            "dictionary fault callback returned",
            origin_context=context,
        )

    def _begin_dictionary_fault(
        self,
        request: _DictionaryFaultRequest,
        context: ExecutionContext,
        meter: _StepMeter,
    ) -> tuple[Word, int]:
        """Enter the callback behind a THROW-discardable fail-closed frame."""

        if request.context is not context:
            raise ExecutionError(
                "dictionary fault crossed into a different execution context"
            )
        if context.returns.has_fault_abort_continuation():
            self._abort_returned_dictionary_fault(context)
        if request.xt == 0:
            self._abort_returned_dictionary_fault(context)
        try:
            target = self.dictionary.resolve(request.xt)
        except KeyError:
            raise ExecutionError(
                "dictionary fault callback is not a live execution token: "
                f"0x{request.xt:016x}"
            ) from None

        context.returns.push_continuation(
            target.xt,
            0,
            fault_abort=True,
        )
        entry_ip = 0
        while True:
            implementation = target.implementation
            if isinstance(implementation, ConstantDefinition):
                meter.tick()
                context.data.push(implementation.value)
                self._abort_returned_dictionary_fault(context)
            if isinstance(implementation, CreatedDefinition):
                meter.tick()
                context.data.push(target.body_address)
                if implementation.action is None:
                    self._abort_returned_dictionary_fault(context)
                target, entry_ip = self._resolve_does_entry(implementation.action)
                break
            if not isinstance(implementation, PrimitiveDefinition):
                break
            meter.tick()
            try:
                invocation = implementation.callback(context)
            except _DictionaryFaultRequest:
                self._abort_returned_dictionary_fault(context)
            if invocation is None:
                self._abort_returned_dictionary_fault(context)
            if not isinstance(invocation, Invoke):
                raise ExecutionError("primitive returned an invalid control result")
            target = self.dictionary.resolve(invocation.xt)

        if not isinstance(target.implementation, ColonDefinition):
            raise ExecutionError(
                f"dictionary fault callback {target.name!r} is not executable"
            )
        return target, entry_ip

    def _execute_top(
        self,
        word: Word | None,
        context: ExecutionContext,
        meter: _StepMeter,
        *,
        root_id: int,
        fault_request: _DictionaryFaultRequest | None = None,
    ) -> None:
        fault_entry = fault_request is not None
        if fault_request is not None:
            target, entry_ip = self._begin_dictionary_fault(
                fault_request,
                context,
                meter,
            )
        else:
            if word is None:
                raise AssertionError("ordinary dispatch requires a target word")
            target = word
            entry_ip = 0
            while True:
                implementation = target.implementation
                if isinstance(implementation, ConstantDefinition):
                    meter.tick()
                    context.data.push(implementation.value)
                    return
                if isinstance(implementation, CreatedDefinition):
                    meter.tick()
                    context.data.push(target.body_address)
                    if implementation.action is None:
                        return
                    target, entry_ip = self._resolve_does_entry(
                        implementation.action
                    )
                    break
                if not isinstance(implementation, PrimitiveDefinition):
                    break
                meter.tick()
                try:
                    invocation = implementation.callback(context)
                except _DictionaryFaultRequest as request:
                    if request.context is not context:
                        raise
                    if self._has_older_dispatch(context):
                        raise
                    target, entry_ip = self._begin_dictionary_fault(
                        request,
                        context,
                        meter,
                    )
                    fault_entry = True
                    break
                if invocation is None:
                    return
                if not isinstance(invocation, Invoke):
                    raise ExecutionError("primitive returned an invalid control result")
                target = self.dictionary.resolve(invocation.xt)

        if not isinstance(target.implementation, ColonDefinition):
            raise ExecutionError(f"word {target.name!r} is not executable")
        if not fault_entry:
            context.returns.push_continuation(
                target.xt,
                entry_ip,
                root=True,
                dispatch_id=root_id,
            )
        current = target
        ip = entry_ip

        while True:
            definition = current.implementation
            if not isinstance(definition, ColonDefinition):
                raise ExecutionError("semantic dispatch entered a non-colon word")
            if not 0 <= ip < len(definition.operations):
                raise ExecutionError(
                    f"instruction pointer {ip} escaped definition {current.name!r}"
                )

            operation = definition.operations[ip]
            meter.tick()

            if isinstance(operation, Literal):
                context.data.push(operation.value)
                ip += 1
            elif isinstance(operation, Call):
                called = self.dictionary.resolve(operation.xt)
                entered = self._call_from_colon(
                    called,
                    caller=current,
                    return_ip=ip + 1,
                    context=context,
                    meter=meter,
                )
                if entered is None:
                    ip += 1
                else:
                    current, ip = entered
            elif isinstance(operation, Branch):
                ip = operation.target
            elif isinstance(operation, BranchZero):
                ip = operation.target if context.data.pop() == 0 else ip + 1
            elif isinstance(operation, RPush):
                context.returns.push(context.data.pop())
                ip += 1
            elif isinstance(operation, RPop):
                context.data.push(context.returns.pop())
                ip += 1
            elif isinstance(operation, RPeek):
                context.data.push(context.returns.peek())
                ip += 1
            elif isinstance(operation, Do):
                index = context.data.pop()
                limit = context.data.pop()
                context.returns.enter_do(limit, index)
                ip += 1
            elif isinstance(operation, QuestionDo):
                index = context.data.pop()
                limit = context.data.pop()
                if index == limit:
                    ip = operation.target
                else:
                    context.returns.enter_do(limit, index)
                    ip += 1
            elif isinstance(operation, Loop):
                ip = operation.target if context.returns.loop() else ip + 1
            elif isinstance(operation, Unloop):
                context.returns.unloop()
                ip += 1
            elif isinstance(operation, InstallDoes):
                latest = self.dictionary.latest_word
                if latest is None or not isinstance(
                    latest.implementation,
                    CreatedDefinition,
                ):
                    raise ExecutionError(
                        "DOES> requires the latest word to be CREATE-family"
                    )
                latest.implementation.action = DoesBodyRef(
                    source_xt=current.xt,
                    entry_ip=operation.entry_ip,
                )
                ip += 1
            elif isinstance(operation, RestoreDataStackPointer):
                context.data.restore_from_top()
                ip += 1
            elif isinstance(operation, RestoreReturnStackPointer):
                pointer = context.data.peek()
                context.returns.set_pointer(pointer)
                context.data.pop()
                ip += 1
            elif isinstance(operation, WriteOutput):
                self.write_uart_bytes(operation.payload)
                ip += 1
            elif isinstance(operation, AbortIf):
                if context.data.pop() != 0:
                    self.write_uart_bytes(operation.payload)
                    context.data.clear()
                    context.returns.clear()
                    raise ForthAbort(
                        'Forth ABORT"',
                        origin_context=context,
                    )
                ip += 1
            elif isinstance(operation, Return):
                continuation = context.returns.pop_continuation()
                if continuation.fault_abort:
                    self._abort_returned_dictionary_fault(context)
                if continuation.root:
                    if continuation.dispatch_id != root_id:
                        raise _GuestControlTransfer(
                            continuation.dispatch_id,
                            context,
                        )
                    return
                caller = self.dictionary.resolve(continuation.xt)
                if not isinstance(caller.implementation, ColonDefinition):
                    raise ExecutionError("continuation does not name a colon word")
                current = caller
                ip = int(continuation.ip)
            else:
                raise ExecutionError(f"unknown semantic operation {operation!r}")

    def _call_from_colon(
        self,
        target: Word,
        *,
        caller: Word,
        return_ip: int,
        context: ExecutionContext,
        meter: _StepMeter,
    ) -> tuple[Word, int] | None:
        while True:
            implementation = target.implementation
            if isinstance(implementation, ConstantDefinition):
                meter.tick()
                context.data.push(implementation.value)
                return None
            if isinstance(implementation, CreatedDefinition):
                meter.tick()
                context.data.push(target.body_address)
                if implementation.action is None:
                    return None
                entered = self._resolve_does_entry(implementation.action)
                context.returns.push_continuation(caller.xt, return_ip)
                return entered
            if not isinstance(implementation, PrimitiveDefinition):
                break
            meter.tick()
            try:
                invocation = implementation.callback(context)
            except _DictionaryFaultRequest as request:
                if request.context is not context:
                    raise
                if self._has_older_dispatch(context):
                    raise
                return self._begin_dictionary_fault(
                    request,
                    context,
                    meter,
                )
            if invocation is None:
                return None
            if not isinstance(invocation, Invoke):
                raise ExecutionError("primitive returned an invalid control result")
            target = self.dictionary.resolve(invocation.xt)

        if not isinstance(target.implementation, ColonDefinition):
            raise ExecutionError(f"word {target.name!r} is not executable")
        context.returns.push_continuation(caller.xt, return_ip)
        return target, 0

    def _resolve_does_entry(self, action: DoesBodyRef) -> tuple[Word, int]:
        source = self.dictionary.resolve(action.source_xt)
        if not isinstance(source.implementation, ColonDefinition):
            raise ExecutionError("DOES> action does not name a colon definition")
        return source, action.entry_ip

    def _resolve_word(self, name_or_xt: bytes | str | int) -> Word:
        if isinstance(name_or_xt, int):
            return self.dictionary.resolve(name_or_xt)
        word = self.dictionary.find(name_or_xt)
        if word is None:
            raise KeyError(f"unknown word {name_or_xt!r}")
        return word

    def _meter_for_public_call(
        self,
        step_budget: int | None,
    ) -> tuple[_StepMeter, int]:
        """Select one meter without letting nested dispatch reset its budget."""

        if self._active_dispatches:
            if step_budget is not None:
                raise ValueError(
                    "nested evaluation or execution cannot replace the active "
                    "step budget"
                )
            meter = self._active_dispatches[-1].meter
            return meter, meter.steps
        meter = _StepMeter(step_budget, self.diagnostics.account_work)
        return meter, 0

    @staticmethod
    def _parse_quoted_literal(
        state: _EvaluationState,
        *,
        unconditionally_skip_next: bool,
    ) -> bytes:
        """Consume one line-local BIOS quote payload and its closing quote."""

        if unconditionally_skip_next:
            state.cursor.consume_byte()
        else:
            state.cursor.consume_byte(ASCII_SPACE)
        return state.cursor.consume_until(ord('"')).data

    def _parse_number(self, token: bytes) -> int | None:
        """Parse exactly the BIOS ``-``/``0x``/``BASE`` number grammar."""

        if not token:
            return None

        position = 0
        negative = token[0] == ord("-")
        if negative:
            position = 1
            if position == len(token):
                return None

        base = self.numeric_base
        if (
            len(token) - position >= 2
            and token[position] == ord("0")
            and token[position + 1] in (ord("x"), ord("X"))
        ):
            base = 16
            position += 2
            if position == len(token):
                return None

        value = 0
        while position < len(token):
            byte = token[position]
            if ord("0") <= byte <= ord("9"):
                digit = byte - ord("0")
            elif ord("A") <= byte <= ord("Z"):
                digit = byte - ord("A") + 10
            elif ord("a") <= byte <= ord("z"):
                digit = byte - ord("a") + 10
            else:
                return None
            if digit >= base:
                return None
            value = u64(value * base + digit)
            position += 1

        return u64(-value) if negative else value

    @staticmethod
    def _token_location(state: _EvaluationState) -> SourceLocation:
        token = state.cursor.last_token
        return state.cursor.location if token is None else token.location

    def _parse_required_word(self, state: _EvaluationState, owner: str) -> bytes:
        value = state.cursor.parse_word()
        if not value:
            self._compile_error(state, f"{owner} requires a following word")
        return value

    def _require_if_frame(
        self,
        state: _EvaluationState,
        *,
        allow_else: bool,
    ) -> _IfFrame:
        compiler = state.compiler
        assert compiler is not None
        if not compiler.controls or not isinstance(compiler.controls[-1], _IfFrame):
            self._compile_error(state, "control word has no matching IF")
        frame = compiler.controls[-1]
        assert isinstance(frame, _IfFrame)
        if frame.has_else and not allow_else:
            self._compile_error(state, "IF already has an ELSE")
        return frame

    def _require_begin_frame(
        self,
        state: _EvaluationState,
        *,
        owner: str,
    ) -> _BeginFrame:
        compiler = state.compiler
        assert compiler is not None
        if not compiler.controls or not isinstance(
            compiler.controls[-1],
            _BeginFrame,
        ):
            self._compile_error(state, f"{owner} has no matching BEGIN")
        frame = compiler.controls[-1]
        assert isinstance(frame, _BeginFrame)
        return frame

    def _require_while_frame(self, state: _EvaluationState) -> _WhileFrame:
        compiler = state.compiler
        assert compiler is not None
        if not compiler.controls or not isinstance(
            compiler.controls[-1],
            _WhileFrame,
        ):
            self._compile_error(state, "REPEAT has no matching WHILE")
        frame = compiler.controls[-1]
        assert isinstance(frame, _WhileFrame)
        return frame

    def _compile_error(self, state: _EvaluationState, message: str) -> None:
        raise SourceError(message, self._token_location(state))


__all__ = [
    "ColonDefinition",
    "ConstantDefinition",
    "CreatedDefinition",
    "DirectiveDefinition",
    "DirectiveKind",
    "DoesBodyRef",
    "EvaluationResult",
    "ExecutionContext",
    "ExecutionResult",
    "Invoke",
    "MegaForthRuntime",
    "PrimitiveCallback",
    "PrimitiveDefinition",
    "WordImplementation",
]
