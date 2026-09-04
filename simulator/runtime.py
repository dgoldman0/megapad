"""Explicit hosted MegaForth compiler and semantic dispatch loop.

The first runtime slice deliberately implements a small real kernel rather
than translating source into Python calls.  Colon continuations remain on the
same ordered return stack as user ``>R`` cells and counted-loop state, and
compiled calls retain the execution token that was bound at compile time.
"""

from __future__ import annotations

import threading
from collections import deque
from contextlib import contextmanager
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, Iterator, NoReturn, TypeAlias, get_args

from shared.cells import CELL_BYTES, MASK64, s64, u64
from shared.crypto_caps import (
    CRYPTO_CAP_CRC_REFLECT_RAW,
    CRYPTO_CAP_KECCAK_F1600,
    CRYPTO_CAP_SHA3_STREAM,
)
from simulator.crc import GuestIdentity, HostedCRCService
from simulator.diagnostics import HostedDiagnosticsService
from simulator.dictionary import Dictionary, DictionaryCheckpoint, Word
from simulator.dictionary_index import (
    DictionaryIndexState,
    HostedDictionaryIndex,
)
from simulator.errors import (
    ExecutionBlocked,
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
    Idle,
    InstallDoes,
    Literal,
    Loop,
    Operation,
    PushStringLiteral,
    QuestionDo,
    RestoreDataStackPointer,
    RestoreReturnStackPointer,
    RPeek,
    RPeekPair,
    RPop,
    RPopPair,
    RPush,
    RPushPair,
    Return,
    Unloop,
    UartReadAttempt,
    WriteOutput,
)
from simulator.kem import HostedKEMService
from simulator.memory import MMIO_BASE, AddressClass, SparseAddressSpace
from simulator.ntt import HostedNTTService
from simulator.platform import (
    HOSTED_CRYPTO_CAPABILITIES,
    OneCorePlatformMMIO,
    SYSINFO_CRYPTO_CAPS,
    SYSINFO_EXTERNAL_BASE,
    SYSINFO_EXTERNAL_SIZE,
    SYSINFO_NUM_CORES,
    SYSINFO_NUM_FULL,
)
from simulator.sha2 import HostedSHA2Service
from simulator.tile import HostedTileService
from simulator.spinlocks import HostedSpinlockBank
from simulator.storage import HostedStorageService
from simulator.source import (
    ASCII_SPACE,
    SourceBuffer,
    SourceCursor,
    SourceLocation,
)
from simulator.stacks import DataStack, ReturnEntry, ReturnStack
from simulator.timer import HostedTimerService
from simulator.terminal_geometry import HostedTerminalGeometryState


_BIOS_EVALUATE_MAX_BYTES = 255
_BIOS_EVALUATE_MAX_DEPTH = 16
_BIOS_EVAL_TOKEN_CAPACITY = 256
_BIOS_SQUOTE_BUFFER_BYTES = 256
_BIOS_SQUOTE_MAX_PAYLOAD = _BIOS_SQUOTE_BUFFER_BYTES - 1


@dataclass(slots=True)
class ExecutionContext:
    """Per-task semantic stacks for one hosted execution context."""

    data: DataStack = field(default_factory=DataStack)
    returns: ReturnStack = field(default_factory=ReturnStack)
    _host_control_fault: str | None = field(default=None, init=False, repr=False)
    _suspension_sequence: int | None = field(default=None, init=False, repr=False)

    @property
    def reusable(self) -> bool:
        """Whether another public dispatch can safely use this context."""

        return (
            self._host_control_fault is None
            and self._suspension_sequence is None
        )

    @property
    def suspended(self) -> bool:
        """Whether a blocked semantic dispatch currently leases this context."""

        return self._suspension_sequence is not None

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
        if self._suspension_sequence is not None:
            raise ExecutionError(
                "execution context is leased by suspended dispatch "
                f"{self._suspension_sequence}"
            )

    def _mark_host_control_fault(self, error: BaseException) -> None:
        if self._host_control_fault is None:
            self._host_control_fault = type(error).__name__

    def _lease_for_suspension(self, sequence: int) -> None:
        if self._suspension_sequence is not None:
            raise AssertionError("execution context already has a suspension lease")
        self._suspension_sequence = sequence

    def _release_suspension(self, sequence: int) -> None:
        if self._suspension_sequence != sequence:
            raise AssertionError("execution context suspension lease is corrupted")
        self._suspension_sequence = None


@dataclass(frozen=True, slots=True)
class Invoke:
    """Primitive request to invoke a dynamic execution token."""

    xt: int

    def __post_init__(self) -> None:
        token = u64(self.xt)
        if token == 0:
            raise ExecutionError("execution token zero is not callable")
        object.__setattr__(self, "xt", token)


class IdleWake(Enum):
    """Host event classes that can release the MP64 ``IDL`` boundary."""

    INTERRUPT = auto()
    DMA = auto()


@dataclass(frozen=True, slots=True)
class ExecutionSuspension:
    """Opaque identity for one runtime-owned blocked dispatch."""

    sequence: int
    _runtime_token: object = field(repr=False)


@dataclass(frozen=True, slots=True)
class IdleWakeReceipt:
    """One-shot runtime receipt for one delivered IDL wake event."""

    kind: IdleWake
    sequence: int
    _suspension_sequence: int = field(repr=False)
    _runtime_token: object = field(repr=False)


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
    TWO_TO_R = auto()
    TWO_R_FROM = auto()
    TWO_R_FETCH = auto()
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
    S_QUOTE = auto()
    ABORT_QUOTE = auto()
    LEFT_BRACKET = auto()
    RIGHT_BRACKET = auto()


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


@dataclass(frozen=True, slots=True)
class BlockedExecution:
    """A semantic dispatch stopped after IDL and awaits an admitted wake."""

    semantic_steps: int
    suspension: ExecutionSuspension


RunResult: TypeAlias = ExecutionResult | BlockedExecution


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
    compile_mode: bool = True
    operations: list[Operation] = field(default_factory=list)
    controls: list[_ControlFrame] = field(default_factory=list)
    literal_pool: bytearray = field(default_factory=bytearray)
    temporary_checkpoint: DictionaryCheckpoint | None = None

    @property
    def temporary(self) -> bool:
        return self.temporary_checkpoint is not None


@dataclass(slots=True)
class _CompilerState:
    """One mutable compiler slot shared only where source semantics require."""

    compiler: _Compiler | None = None


@dataclass(slots=True)
class _EvaluationState:
    context: ExecutionContext
    cursor: SourceCursor
    meter: _StepMeter
    _compiler_state: _CompilerState = field(default_factory=_CompilerState)
    token_count: int = 0
    definitions: list[Word] = field(default_factory=list)
    bios_evaluator: bool = False

    @property
    def compiler(self) -> _Compiler | None:
        return self._compiler_state.compiler

    @compiler.setter
    def compiler(self, value: _Compiler | None) -> None:
        self._compiler_state.compiler = value


@dataclass(frozen=True, slots=True)
class _BiosEvaluationFrame:
    """One input context retained until normal return or explicit unwind."""

    context: ExecutionContext


@dataclass(slots=True)
class _BiosEvaluatorState:
    """Runtime-owned state behind the mutable semantic BIOS evaluator ABI."""

    status_address: int
    line_address: int
    column_address: int
    depth_address: int
    throw_address: int
    token_address: int
    token_length: int = 0
    compiler_state: _CompilerState = field(default_factory=_CompilerState)
    frames: list[_BiosEvaluationFrame] = field(default_factory=list)


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


@dataclass(frozen=True, slots=True)
class _DispatchCursor:
    """Resume location immediately after one semantic IDL operation."""

    xt: int
    ip: int


@dataclass(slots=True)
class _SuspendedExecution:
    """Runtime-owned continuation and guard state for one blocked dispatch."""

    handle: ExecutionSuspension
    context: ExecutionContext
    meter: _StepMeter
    starting_steps: int
    root_id: int
    cursor: _DispatchCursor
    return_snapshot: tuple[ReturnEntry, ...]
    capture_checkpoint: int
    had_pointer_capture: bool
    blocked_data_snapshot: tuple[int, ...]
    blocked_return_snapshot: tuple[ReturnEntry, ...]
    wake_receipt: IdleWakeReceipt | None = None


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


class _UndefinedWord(SourceError):
    """Narrow source failure translated by the guest checked evaluator."""

    def __init__(self, token: bytes, location: SourceLocation) -> None:
        self.token = token
        super().__init__(f"unknown word {token!r}", location)


class MegaForthRuntime:
    """Hosted dictionary, evaluator, and explicit semantic dispatcher."""

    def __init__(
        self,
        *,
        dictionary_start: int = 0x1000,
        memory: SparseAddressSpace | None = None,
        diagnostics: HostedDiagnosticsService | None = None,
        timer: HostedTimerService | None = None,
        storage: HostedStorageService | None = None,
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
        if timer is not None and not isinstance(timer, HostedTimerService):
            raise TypeError("timer must be a HostedTimerService or None")
        if storage is not None and not isinstance(storage, HostedStorageService):
            raise TypeError("storage must be a HostedStorageService or None")
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
        self.rtc = platform_mmio.rtc
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
        self.timer = (
            HostedTimerService()
            if timer is None
            else timer.clone()
        )
        self.storage = HostedStorageService() if storage is None else storage
        self.tile = HostedTileService(
            self.memory,
            self.field,
            account_operation=self.diagnostics.account_tile_operation,
        )
        self._runtime_token = object()
        self._session_owner_lock = threading.RLock()
        self._session_owner_token: object | None = None
        self._session_owner_thread: int | None = None
        self._legacy_terminal_geometry = HostedTerminalGeometryState()
        self._session_terminal_geometry: HostedTerminalGeometryState | None = None
        self._next_suspension_sequence = 1
        self._next_wake_sequence = 1
        self._suspended_execution: _SuspendedExecution | None = None
        self.dictionary = Dictionary(
            start_address=dictionary_start,
            memory=self.memory,
            mutation_guard=self._require_no_suspension,
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
        self._external = next(
            (
                region
                for region in self.memory.regions
                if region.kind is AddressClass.EXTERNAL
            ),
            None,
        )
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
        # Hardware user mode was removed from MegaPad, but its public BIOS
        # compatibility surface still exposes inert MPU registers.  Preserve
        # their guest-visible state without using it to restrict semantic
        # memory access; PRIV@ therefore remains permanently supervisor (0).
        self._mpu_base = 0
        self._mpu_limit = 0
        self._provided: set[bytes] = set()
        self._active_input_states: list[_EvaluationState] = []
        self._bios_evaluator: _BiosEvaluatorState | None = None
        self._active_dispatches: list[_DispatchFrame] = []
        self._transient_words: dict[int, Word] = {}
        self._next_dispatch_root_id = 1
        self._uart_input: deque[int] = deque()
        self._uart_output = bytearray()
        if install_core_words:
            from simulator.core_words import install_core

            install_core(self)
        # BIOS S" interpret mode owns one reusable 255-byte payload plus its
        # terminator. Keep it in the protected Bank-0 prefix rather than at
        # transient HERE, so later definitions cannot change its address.
        self._squote_buffer_address = self.dictionary.here
        self._preflight_dictionary_growth(
            _BIOS_SQUOTE_BUFFER_BYTES,
            self.main_context,
        )
        self.dictionary.allot(_BIOS_SQUOTE_BUFFER_BYTES)
        self.memory.fill(
            self._squote_buffer_address,
            _BIOS_SQUOTE_BUFFER_BYTES,
            0,
        )
        self.dictionary.protect_current_prefix_from_numeric_rollback()
        self.storage.claim()

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

        self._require_session_owner_access("revoke a provided module")
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
    def privilege_level(self) -> int:
        """Return the retired hardware privilege level, always supervisor."""

        return 0

    @property
    def mpu_base(self) -> int:
        """Return the inert guest-visible MPU lower-bound register."""

        return self._mpu_base

    @property
    def mpu_limit(self) -> int:
        """Return the inert guest-visible MPU exclusive-limit register."""

        return self._mpu_limit

    @property
    def dictionary_index_state(self) -> DictionaryIndexState:
        """Return stable diagnostics for the caller-backed BIOS index."""

        return self.dictionary_index.state

    @property
    def uart_input(self) -> bytes:
        """Return an immutable snapshot of bytes waiting at the hosted UART."""

        return bytes(self._uart_input)

    @property
    def uart_input_available(self) -> bool:
        """Whether at least one byte is waiting in the hosted UART RX FIFO."""

        return bool(self._uart_input)

    @property
    def uart_input_pending(self) -> int:
        """Return the number of bytes waiting in the hosted UART RX FIFO."""

        return len(self._uart_input)

    def inject_uart_input(self, payload: bytes) -> None:
        """Append one validated byte string to the hosted UART RX FIFO."""

        with self._session_owner_lock:
            self._require_unowned_host_access("inject UART input")
            self._append_uart_input(payload)

    def _append_uart_input(self, payload: bytes) -> None:
        if not isinstance(payload, bytes):
            raise TypeError("UART input payload must be bytes")
        self._uart_input.extend(payload)

    def _session_inject_uart_input(self, token: object, payload: bytes) -> None:
        """Append UART input for the exact owning session backend."""

        with self._session_owner_lock:
            self._require_session_owner_token(token)
            self._append_uart_input(payload)

    def discard_uart_input_tail(self, count: int) -> None:
        """Discard exactly ``count`` most-recently queued UART RX bytes."""

        with self._session_owner_lock:
            self._require_unowned_host_access("discard UART input")
            self._discard_uart_input_tail(count)

    def _discard_uart_input_tail(self, count: int) -> None:
        if isinstance(count, bool) or not isinstance(count, int):
            raise TypeError("UART input discard count must be an integer")
        if count < 0:
            raise ValueError("UART input discard count must not be negative")
        if count > len(self._uart_input):
            raise ValueError("UART input discard count exceeds pending input")
        for _ in range(count):
            self._uart_input.pop()

    def _session_discard_uart_input_tail(
        self,
        token: object,
        count: int,
    ) -> None:
        """Discard an RX suffix for the exact owning session backend."""

        with self._session_owner_lock:
            self._require_session_owner_token(token)
            self._discard_uart_input_tail(count)

    def _take_uart_input_byte(self) -> int | None:
        """Consume one queued UART byte, or report that RX remains empty."""

        if not self._uart_input:
            return None
        return self._uart_input.popleft()

    @property
    def uart_output(self) -> bytes:
        """Return an immutable snapshot of bytes written to the hosted UART."""

        return bytes(self._uart_output)

    def write_uart_bytes(self, payload: bytes) -> None:
        """Append one validated byte string to the hosted UART stream."""

        with self._session_owner_lock:
            self._require_session_owner_access("write UART output")
            if not isinstance(payload, bytes):
                raise TypeError("UART output payload must be bytes")
            self._uart_output.extend(payload)

    def flush_uart_output(self) -> None:
        """Complete the hosted UART's current output publication.

        Hosted writes enter the runtime-owned byte stream synchronously, so
        there is no lower FIFO or shifter to wait for. Keeping an explicit
        operation preserves the source-visible ``TX-FLUSH`` boundary for a
        later transport adapter without inventing machine timing here.
        """

        with self._session_owner_lock:
            self._require_session_owner_access("flush UART output")

    def drain_uart_output(self) -> bytes:
        """Return all pending UART bytes and clear the runtime-owned buffer."""

        with self._session_owner_lock:
            self._require_unowned_host_access("drain UART output")
            return self._drain_uart_output()

    def _drain_uart_output(self) -> bytes:
        payload = bytes(self._uart_output)
        self._uart_output.clear()
        return payload

    def _session_drain_uart_output(self, token: object) -> bytes:
        """Drain UART output for the exact owning session backend."""

        with self._session_owner_lock:
            self._require_session_owner_token(token)
            return self._drain_uart_output()

    def _claim_session_owner(
        self,
        token: object,
        *,
        terminal_geometry: HostedTerminalGeometryState | None = None,
    ) -> None:
        """Grant one backend exclusive public UART and dispatch ownership."""

        with self._session_owner_lock:
            if token is None:
                raise TypeError("session owner token must not be None")
            if terminal_geometry is not None and not isinstance(
                terminal_geometry,
                HostedTerminalGeometryState,
            ):
                raise TypeError(
                    "terminal_geometry must be HostedTerminalGeometryState or None"
                )
            if self._session_owner_token is not None:
                raise ExecutionError("hosted runtime already has a session owner")
            if (
                self._active_dispatches
                or self._active_input_states
                or self._suspended_execution is not None
            ):
                raise ExecutionError(
                    "cannot acquire session ownership during semantic execution"
                )
            self._session_owner_token = token
            self._session_terminal_geometry = terminal_geometry

    def _release_session_owner(self, token: object) -> None:
        """Release the exact backend's exclusive runtime ownership."""

        with self._session_owner_lock:
            self._require_session_owner_token(token)
            if self._session_owner_thread is not None:
                raise ExecutionError("cannot release an active session boundary")
            if self._suspended_execution is not None:
                raise ExecutionError(
                    "cannot release a suspended semantic dispatch"
                )
            terminal_geometry = self._session_terminal_geometry
            if terminal_geometry is not None:
                self._legacy_terminal_geometry.restore(
                    terminal_geometry.snapshot()
                )
            self._session_terminal_geometry = None
            self._session_owner_token = None

    def _active_terminal_geometry_locked(self) -> HostedTerminalGeometryState:
        geometry = self._session_terminal_geometry
        return self._legacy_terminal_geometry if geometry is None else geometry

    def terminal_columns(self) -> int:
        """Return the public BIOS column count for the active owner boundary."""

        with self._session_owner_lock:
            self._require_session_owner_access("read terminal columns")
            return self._active_terminal_geometry_locked().snapshot().cols

    def terminal_rows(self) -> int:
        """Return the public BIOS row count for the active owner boundary."""

        with self._session_owner_lock:
            self._require_session_owner_access("read terminal rows")
            return self._active_terminal_geometry_locked().snapshot().rows

    def terminal_size(self) -> tuple[int, int]:
        """Return one coherent public BIOS terminal-dimension snapshot."""

        with self._session_owner_lock:
            self._require_session_owner_access("read terminal size")
            snapshot = self._active_terminal_geometry_locked().snapshot()
            return snapshot.cols, snapshot.rows

    def consume_terminal_resized(self) -> bool:
        """Return and clear the public BIOS resize flag for the active owner."""

        with self._session_owner_lock:
            self._require_session_owner_access("read terminal resize state")
            return self._active_terminal_geometry_locked().consume_resized()

    def consume_terminal_resize_denied(self) -> bool:
        """Return and clear the public BIOS request-denied flag."""

        with self._session_owner_lock:
            self._require_session_owner_access(
                "read terminal resize-denied state"
            )
            return self._active_terminal_geometry_locked().consume_resize_denied()

    def request_terminal_resize(self, cols: int, rows: int) -> None:
        """Publish one asynchronous guest resize request to the active host."""

        with self._session_owner_lock:
            self._require_session_owner_access("request a terminal resize")
            self._active_terminal_geometry_locked().request_resize(cols, rows)

    def set_terminal_geometry(self, cols: int, rows: int) -> None:
        """Set fixed legacy geometry while no session backend owns the runtime."""

        with self._session_owner_lock:
            self._require_unowned_host_access("set terminal geometry")
            self._legacy_terminal_geometry.apply(cols, rows)

    @contextmanager
    def _session_owner_scope(self, token: object) -> Iterator[None]:
        """Admit public runtime effects for one backend-owned boundary."""

        with self._session_owner_lock:
            self._require_session_owner_token(token)
            if self._session_owner_thread is not None:
                raise ExecutionError("a runtime session boundary is already active")
            thread_id = threading.get_ident()
            self._session_owner_thread = thread_id
        try:
            yield
        finally:
            with self._session_owner_lock:
                if self._session_owner_thread != thread_id:
                    raise AssertionError(
                        "runtime session boundary ownership changed"
                    )
                self._session_owner_thread = None

    def _require_session_owner_token(self, token: object) -> None:
        with self._session_owner_lock:
            if self._session_owner_token is not token:
                raise ExecutionError(
                    "runtime session owner token is stale or foreign"
                )

    def _require_session_owner_access(self, operation: str) -> None:
        with self._session_owner_lock:
            if (
                self._session_owner_token is not None
                and self._session_owner_thread != threading.get_ident()
            ):
                raise ExecutionError(
                    f"cannot {operation} outside the owning session boundary"
                )

    def _require_unowned_host_access(self, operation: str) -> None:
        """Reject host-side FIFO ownership bypass during an owned session."""

        with self._session_owner_lock:
            if self._session_owner_token is not None:
                raise ExecutionError(
                    f"cannot {operation} outside the owning session boundary"
                )

    def set_numeric_base(self, base: int) -> None:
        """Set the source number base as a wrapped guest cell.

        The BIOS exposes ``BASE`` as a mutable cell and does not constrain its
        contents.  A core-less runtime retains a bootstrap value; the ordinary
        semantic BIOS binds this method to the live guest cell.
        """

        self._require_session_owner_access("change the numeric base")
        value = u64(base)
        if self._numeric_base_address is None:
            self._bootstrap_numeric_base = value
        else:
            self.memory.write64(self._numeric_base_address, value)

    def set_mpu_base(self, base: int) -> None:
        """Store one wrapped cell in the non-enforcing MPU base register."""

        self._require_session_owner_access("change the MPU base")
        self._mpu_base = u64(base)

    def set_mpu_limit(self, limit: int) -> None:
        """Store one wrapped cell in the non-enforcing MPU limit register."""

        self._require_session_owner_access("change the MPU limit")
        self._mpu_limit = u64(limit)

    def bind_numeric_base_address(self, address: int) -> None:
        """Bind numeric parsing and printers to the semantic BIOS BASE cell."""

        self._require_session_owner_access("bind the numeric base")
        if self._numeric_base_address is not None:
            raise RuntimeError("numeric BASE is already bound")
        self.memory.write64(address, self._bootstrap_numeric_base)
        self._numeric_base_address = address

    def bind_bios_evaluator(
        self,
        *,
        status_address: int,
        line_address: int,
        column_address: int,
        depth_address: int,
        throw_address: int,
        token_address: int,
    ) -> None:
        """Bind the runtime-owned evaluator to its protected BIOS storage."""

        self._require_session_owner_access("bind the BIOS evaluator")
        if self._bios_evaluator is not None:
            raise RuntimeError("semantic BIOS evaluator is already bound")
        cell_addresses = (
            status_address,
            line_address,
            column_address,
            depth_address,
            throw_address,
        )
        if any(
            not isinstance(address, int) or not 0 <= address <= MASK64
            for address in (*cell_addresses, token_address)
        ):
            raise ValueError("evaluator storage addresses must be uint64 values")
        if len(set(cell_addresses)) != len(cell_addresses):
            raise ValueError("evaluator diagnostic cells must not alias")
        if token_address > MASK64 - (_BIOS_EVAL_TOKEN_CAPACITY - 1):
            raise ValueError("evaluator token buffer wraps uint64")

        for address in cell_addresses:
            self.memory.write64(address, 0)
        self.memory.fill(token_address, _BIOS_EVAL_TOKEN_CAPACITY, 0)
        self._bios_evaluator = _BiosEvaluatorState(
            status_address=status_address,
            line_address=line_address,
            column_address=column_address,
            depth_address=depth_address,
            throw_address=throw_address,
            token_address=token_address,
        )

    def bios_evaluate(self, context: ExecutionContext) -> None:
        """Execute the legacy guest ``EVALUATE ( addr len -- )`` ABI."""

        with self._session_owner_lock:
            self._require_session_owner_access("run the BIOS evaluator")
            if not isinstance(context, ExecutionContext):
                raise TypeError("context must be an ExecutionContext")
            evaluator = self._require_bios_evaluator()
            try:
                self._bios_evaluate(evaluator, context)
            except (_GuestControlTransfer, _DictionaryFaultRequest):
                # Both paths have abandoned the Python input cursor while guest
                # control is still entitled to reconstruct logical EVALUATE depth.
                raise
            except BaseException:
                self._fail_closed_bios_evaluator(evaluator)
                raise

    def bios_evaluate_checked(self, context: ExecutionContext) -> None:
        """Run the early BIOS checked wrapper and append its sticky status."""

        self.bios_evaluate(context)
        evaluator = self._require_bios_evaluator()
        context.data.push(self.memory.read64(evaluator.status_address))

    def bios_evaluate_finish(self, context: ExecutionContext) -> None:
        """Report whether the persistent guest compiler is completely idle."""

        self._require_session_owner_access("finish BIOS evaluation")
        if not isinstance(context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        evaluator = self._require_bios_evaluator()
        self._clear_bios_evaluator_diagnostics(evaluator)
        status = 4 if evaluator.compiler_state.compiler is not None else 0
        if status:
            self.memory.write64(evaluator.status_address, status)
        context.data.push(status)

    def bios_evaluator_reset(self) -> None:
        """Discard only persistent guest compiler bookkeeping."""

        self._require_session_owner_access("reset the BIOS evaluator")
        evaluator = self._require_bios_evaluator()
        compiler = evaluator.compiler_state.compiler
        evaluator.compiler_state.compiler = None
        if compiler is not None and compiler.temporary:
            self._discard_temporary_compiler(compiler)

    def bios_evaluator_unwind(self, context: ExecutionContext) -> None:
        """Discard abandoned logical input frames down to one checkpoint."""

        self._require_session_owner_access("unwind BIOS evaluation")
        if not isinstance(context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        evaluator = self._require_bios_evaluator()
        target_cell = context.data.pop()
        if s64(target_cell) < 0:
            return
        depth = self._validated_bios_evaluator_depth(evaluator)
        if target_cell > depth:
            return
        del evaluator.frames[target_cell:]
        self.memory.write64(evaluator.depth_address, target_cell)

    def bios_eval_status(self, context: ExecutionContext) -> None:
        """Push the mutable ``EVAL-STATUS`` diagnostic cell address."""

        self._require_session_owner_access("read BIOS evaluator status")
        context.data.push(self._require_bios_evaluator().status_address)

    def bios_eval_line(self, context: ExecutionContext) -> None:
        """Push the caller-owned one-based ``EVAL-LINE`` cell address."""

        self._require_session_owner_access("read BIOS evaluator line")
        context.data.push(self._require_bios_evaluator().line_address)

    def bios_eval_column(self, context: ExecutionContext) -> None:
        """Push the zero-based ``EVAL-COLUMN`` diagnostic cell address."""

        self._require_session_owner_access("read BIOS evaluator column")
        context.data.push(self._require_bios_evaluator().column_address)

    def bios_eval_depth(self, context: ExecutionContext) -> None:
        """Push the mutable logical ``EVAL-DEPTH`` cell address."""

        self._require_session_owner_access("read BIOS evaluator depth")
        context.data.push(self._require_bios_evaluator().depth_address)

    def bios_eval_throw(self, context: ExecutionContext) -> None:
        """Push the KDOS-owned ``EVAL-THROW`` diagnostic cell address."""

        self._require_session_owner_access("read BIOS evaluator throw")
        context.data.push(self._require_bios_evaluator().throw_address)

    def bios_eval_token(self, context: ExecutionContext) -> None:
        """Return the stable first-failure token buffer and current length."""

        self._require_session_owner_access("read the BIOS evaluator token")
        evaluator = self._require_bios_evaluator()
        context.data.push(evaluator.token_address)
        context.data.push(evaluator.token_length)

    def _bios_evaluate(
        self,
        evaluator: _BiosEvaluatorState,
        context: ExecutionContext,
    ) -> None:
        depth = self._validated_bios_evaluator_depth(evaluator)
        if depth == 0:
            self._clear_bios_evaluator_diagnostics(evaluator)

        if depth and self.memory.read64(evaluator.status_address) != 0:
            context.data.pop()
            context.data.pop()
            return

        if depth >= _BIOS_EVALUATE_MAX_DEPTH:
            context.data.pop()
            context.data.pop()
            self.memory.write64(evaluator.status_address, 3)
            self.memory.write64(evaluator.column_address, 0)
            evaluator.token_length = 0
            self.write_uart_bytes(b"EVALUATE depth limit exceeded\n")
            return

        length = context.data.pop()
        address = context.data.pop()
        if length > _BIOS_EVALUATE_MAX_BYTES:
            self.memory.write64(evaluator.status_address, 2)
            self.memory.write64(evaluator.column_address, 0)
            evaluator.token_length = 0
            self.write_uart_bytes(b"EVALUATE input exceeds 255 bytes\n")
            return

        source = self.memory.read_bytes(address, length)
        cursor = SourceCursor(source, source_name="<EVALUATE>")
        frame = _BiosEvaluationFrame(context)
        evaluator.frames.append(frame)
        self.memory.write64(evaluator.depth_address, len(evaluator.frames))
        meter, _starting_steps = self._meter_for_public_call(None)
        state = _EvaluationState(
            context,
            cursor,
            meter,
            _compiler_state=evaluator.compiler_state,
            bios_evaluator=True,
        )
        try:
            self._evaluate_line(state)
        except _UndefinedWord as error:
            self._capture_bios_undefined(evaluator, error)
            self._pop_bios_evaluator_frame(evaluator, frame)
        except (_GuestControlTransfer, _DictionaryFaultRequest):
            # The Python cursor is gone, but the native input frame would
            # remain abandoned until KDOS calls EVALUATOR-UNWIND.
            raise
        else:
            self._pop_bios_evaluator_frame(evaluator, frame)

    def _capture_bios_undefined(
        self,
        evaluator: _BiosEvaluatorState,
        error: _UndefinedWord,
    ) -> None:
        token = error.token
        if len(token) > _BIOS_EVALUATE_MAX_BYTES:
            raise AssertionError("evaluator token exceeds its admitted input")
        self.memory.write64(evaluator.status_address, 1)
        self.memory.write64(evaluator.column_address, error.location.column)
        self.memory.write_bytes(evaluator.token_address, token)
        evaluator.token_length = len(token)
        self.write_uart_bytes(token + b" ? (not found)\n")

    def _clear_bios_evaluator_diagnostics(
        self,
        evaluator: _BiosEvaluatorState,
    ) -> None:
        self.memory.write64(evaluator.status_address, 0)
        self.memory.write64(evaluator.column_address, 0)
        self.memory.write64(evaluator.throw_address, 0)
        evaluator.token_length = 0

    def _validated_bios_evaluator_depth(
        self,
        evaluator: _BiosEvaluatorState,
    ) -> int:
        depth = self.memory.read64(evaluator.depth_address)
        if depth != len(evaluator.frames):
            self._fail_closed_bios_evaluator(evaluator)
            raise ExecutionError(
                "guest EVAL-DEPTH no longer matches hosted input frames"
            )
        return depth

    def _pop_bios_evaluator_frame(
        self,
        evaluator: _BiosEvaluatorState,
        frame: _BiosEvaluationFrame,
    ) -> None:
        if not evaluator.frames or evaluator.frames[-1] is not frame:
            self._fail_closed_bios_evaluator(evaluator)
            raise ExecutionError("hosted EVALUATE frame stack is corrupted")
        evaluator.frames.pop()
        self.memory.write64(evaluator.depth_address, len(evaluator.frames))

    def _fail_closed_bios_evaluator(
        self,
        evaluator: _BiosEvaluatorState,
    ) -> None:
        evaluator.frames.clear()
        compiler = evaluator.compiler_state.compiler
        evaluator.compiler_state.compiler = None
        if compiler is not None and compiler.temporary:
            self._discard_temporary_compiler(compiler)
        self.memory.write64(evaluator.depth_address, 0)

    def _fail_closed_active_bios_evaluator(self) -> None:
        evaluator = self._bios_evaluator
        if evaluator is not None and (
            evaluator.frames
            or evaluator.compiler_state.compiler is not None
        ):
            self._fail_closed_bios_evaluator(evaluator)

    def _require_bios_evaluator(self) -> _BiosEvaluatorState:
        evaluator = self._bios_evaluator
        if evaluator is None:
            raise ExecutionError("semantic BIOS evaluator is not installed")
        return evaluator

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

        self._require_session_owner_access("change the dictionary fault callback")
        self._require_no_suspension("change the dictionary fault callback")
        self._dictionary_fault_xt = u64(xt)

    def configure_dictionary_bounds(
        self,
        base: int,
        limit: int,
        context: ExecutionContext,
    ) -> None:
        """Install one checked inclusive/exclusive external dictionary zone."""

        self._require_session_owner_access("change dictionary bounds")
        self._require_no_suspension("change dictionary bounds")
        base = u64(base)
        limit = u64(limit)
        if base == 0 and limit == 0:
            self.disable_dictionary_bounds()
            return

        advertised_base = self.memory.read64(
            MMIO_BASE + SYSINFO_EXTERNAL_BASE
        )
        advertised_size = self.memory.read64(
            MMIO_BASE + SYSINFO_EXTERNAL_SIZE
        )
        advertised_end = advertised_base + advertised_size
        valid = (
            base != 0
            and limit > base
            and advertised_size != 0
            and advertised_end <= MASK64
            and advertised_end > advertised_base
            and base >= advertised_base
            and limit <= advertised_end
            and self._external is not None
            and self._external.base == advertised_base
            and self._external.limit == advertised_end
        )
        if not valid:
            self._request_dictionary_fault(
                context,
                "invalid external dictionary bounds",
            )

        # Limit is the active marker in BIOS.  Disable it before changing the
        # selected physical zone or base, then publish the complete new pair.
        self._dictionary_limit = 0
        active_floor, active_limit = self.dictionary.active_zone
        operating_in_external = (
            self._external is not None
            and self._external.base <= active_floor
            and active_limit <= self._external.limit
        )
        if operating_in_external and base <= self.dictionary.here <= limit:
            self.dictionary.move_here(
                self.dictionary.here,
                floor=base,
                limit=limit,
            )
        self._dictionary_base = base
        self._dictionary_limit = limit

    def disable_dictionary_bounds(self) -> None:
        """Restore guarded Bank-0 dictionary allocation without moving HERE."""

        self._require_session_owner_access("disable dictionary bounds")
        self._require_no_suspension("disable dictionary bounds")
        self._dictionary_limit = 0
        self._dictionary_base = 0

    def configure_dictionary_index(self, base: int, slots: int) -> int:
        """Install, rebuild, or disable the caller-backed BIOS index."""

        self._require_session_owner_access("reconfigure the dictionary index")
        self._require_no_suspension("reconfigure the dictionary index")
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

    def _has_active_guest_transfer_target(
        self,
        transfer: _GuestControlTransfer,
    ) -> bool:
        """Whether an internal transfer can still reach its exact guest root."""

        return any(
            frame.context is transfer.context
            and frame.root_id == transfer.root_id
            for frame in self._active_dispatches
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

    def _allocate_suspension_handle(self) -> ExecutionSuspension:
        sequence = self._next_suspension_sequence
        self._next_suspension_sequence += 1
        if sequence > MASK64:
            raise ExecutionError("semantic suspension identity space exhausted")
        return ExecutionSuspension(sequence, self._runtime_token)

    def _require_no_suspension(self, operation: str) -> None:
        suspended = self._suspended_execution
        if suspended is not None:
            raise ExecutionError(
                f"cannot {operation} while dispatch "
                f"{suspended.handle.sequence} is suspended"
            )

    def _require_suspension(
        self,
        handle: ExecutionSuspension,
    ) -> _SuspendedExecution:
        if not isinstance(handle, ExecutionSuspension):
            raise TypeError("suspension must be an ExecutionSuspension")
        if handle._runtime_token is not self._runtime_token:
            raise ExecutionError("suspension belongs to a different runtime")
        suspended = self._suspended_execution
        if suspended is None or suspended.handle is not handle:
            raise ExecutionError("suspension is stale or already consumed")
        return suspended

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
        self._require_no_suspension("mutate the dictionary")
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
        self._require_no_suspension("move the dictionary frontier")
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
            _StepMeter(None, self._account_semantic_step),
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
        self._require_session_owner_access("allot dictionary storage")
        if not isinstance(delta_cell, int):
            self.dictionary.allot(delta_cell)
            return
        target = self.dictionary.here + s64(delta_cell)
        self._preflight_dictionary_target(target, context)
        if self._dictionary_limit:
            floor = self._dictionary_base
            limit = self._dictionary_limit
        else:
            floor = self.dictionary.start_address
            limit = self._bank0.limit
        try:
            self.dictionary.move_here(target, floor=floor, limit=limit)
        except (OverflowError, ValueError) as exc:
            self._request_dictionary_fault(context, str(exc))

    def comma_dictionary(self, cell: int, context: ExecutionContext) -> None:
        self._require_session_owner_access("compile a dictionary cell")
        self._preflight_dictionary_growth(CELL_BYTES, context)
        try:
            self.dictionary.comma(cell)
        except OverflowError as exc:
            self._request_dictionary_fault(context, str(exc))

    def c_comma_dictionary(self, cell: int, context: ExecutionContext) -> None:
        self._require_session_owner_access("compile a dictionary byte")
        if self._active_input_states:
            state = self._active_input_states[-1]
            compiler = state.compiler
            if (
                state.context is context
                and compiler is not None
                and not compiler.compile_mode
            ):
                byte = cell & 0xFF
                if byte != 0:
                    self._compile_error(
                        state,
                        "hosted raw opcode emission supports only MP64 IDL byte 0",
                    )
                compiler.operations.append(Idle())
                return
        self._preflight_dictionary_growth(1, context)
        try:
            self.dictionary.c_comma(cell)
        except OverflowError as exc:
            self._request_dictionary_fault(context, str(exc))

    def tile_align_dictionary(self, context: ExecutionContext) -> None:
        """Apply BIOS ``TALIGN`` growth semantics to the hosted frontier."""

        self._require_session_owner_access("align the dictionary")
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
        self._require_session_owner_access("roll back the dictionary")
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
        initial_body: bytes = b"",
    ) -> Word:
        self._require_session_owner_access("define a primitive")
        if not callable(callback):
            raise TypeError("primitive callback must be callable")
        return self._define_public_dictionary_word(
            name,
            PrimitiveDefinition(callback),
            immediate=immediate,
            initial_body=initial_body,
        )

    def define_colon(
        self,
        name: bytes | str,
        operations: tuple[Operation, ...],
        *,
        immediate: bool = False,
        literal_pool: bytes = b"",
    ) -> Word:
        """Publish one trusted, complete hosted-IR colon definition."""

        self._require_session_owner_access("define a colon word")
        if not isinstance(operations, tuple):
            raise TypeError("colon operations must be a tuple")
        if not isinstance(literal_pool, bytes):
            raise TypeError("colon literal pool must be bytes")
        if not operations:
            raise ValueError("colon operations must not be empty")
        operation_types = get_args(Operation)
        if any(
            not isinstance(operation, operation_types)
            for operation in operations
        ):
            raise TypeError("colon operations contain an unsupported operation")
        if not isinstance(operations[-1], Return):
            raise ValueError("colon operations must end with Return")
        operation_count = len(operations)
        for operation in operations:
            if isinstance(operation, (Branch, BranchZero, QuestionDo, Loop)):
                if operation.target >= operation_count:
                    raise ValueError("colon branch target escapes its definition")
            elif isinstance(operation, InstallDoes):
                if operation.entry_ip >= operation_count:
                    raise ValueError("DOES> entry point escapes its definition")
            elif isinstance(operation, PushStringLiteral):
                terminator = operation.offset + operation.length
                if (
                    terminator >= len(literal_pool)
                    or literal_pool[terminator] != 0
                ):
                    raise ValueError(
                        "colon string literal escapes its NUL-terminated body pool"
                    )
        return self._define_public_dictionary_word(
            name,
            ColonDefinition(operations),
            immediate=immediate,
            initial_body=literal_pool,
        )

    def define_constant(self, name: bytes | str, value: int) -> Word:
        """Publish an executable cell constant under one stable XT.

        Constants created by a defining primitive during :meth:`evaluate` are
        also included in that evaluation's ordered definition ledger.
        """

        self._require_session_owner_access("define a constant")
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

        self._require_session_owner_access("define a created word")
        word = self._define_public_dictionary_word(
            name,
            CreatedDefinition(),
            initial_body=initial_body,
        )
        if self._active_input_states:
            self._active_input_states[-1].definitions.append(word)
        return word

    def define_directive(
        self,
        name: bytes | str,
        kind: DirectiveKind,
        *,
        immediate: bool = True,
    ) -> Word:
        self._require_session_owner_access("define a directive")
        if not isinstance(kind, DirectiveKind):
            raise TypeError("directive kind must be a DirectiveKind")
        if not isinstance(immediate, bool):
            raise TypeError("directive immediacy must be a bool")
        return self._define_public_dictionary_word(
            name,
            DirectiveDefinition(kind),
            immediate=immediate,
        )

    def parse_input_word(self) -> bytes:
        """Parse an optional word from the active physical input line."""

        self._require_session_owner_access("parse the active input")
        if not self._active_input_states:
            raise ExecutionError("cannot parse a word without an active input line")
        return self._active_input_states[-1].cursor.parse_word()

    def parse_word_to_dictionary_tail(self, delimiter: int) -> int:
        """Publish one BIOS ``WORD`` value and then commit its input cursor."""

        self._require_session_owner_access("parse into the dictionary")
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

        self._require_session_owner_access("parse required input")
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

        with self._session_owner_lock:
            self._require_session_owner_access("evaluate source")
            try:
                return self._evaluate_source(
                    source,
                    source_name=source_name,
                    context=context,
                    step_budget=step_budget,
                )
            except _GuestControlTransfer as transfer:
                if not self._has_active_guest_transfer_target(transfer):
                    self._fail_closed_active_bios_evaluator()
                raise
            except _DictionaryFaultRequest as request:
                if not self._has_active_dispatch(request.context):
                    self._fail_closed_active_bios_evaluator()
                raise
            except BaseException:
                self._fail_closed_active_bios_evaluator()
                raise

    def _evaluate_source(
        self,
        source: bytes | bytearray | memoryview,
        *,
        source_name: str,
        context: ExecutionContext | None,
        step_budget: int | None,
    ) -> EvaluationResult:
        """Implement :meth:`evaluate` below its outer host-escape guard."""

        active_context = self.main_context if context is None else context
        if not isinstance(active_context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        self._require_no_suspension("evaluate source")
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
                if state.compiler.temporary:
                    compiler = state.compiler
                    location = compiler.location
                    state.compiler = None
                    self._discard_temporary_compiler(compiler, state)
                    raise SourceError(
                        "interpret IF has no terminating THEN",
                        location,
                    )
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
            self._fail_closed_active_bios_evaluator()
            if active_context.returns.has_pointer_captures_after(
                capture_checkpoint
            ):
                active_context._mark_host_control_fault(exc)
            if exc.bind_origin(active_context):
                active_context.data.clear()
                active_context.returns.clear()
            raise
        except BaseException as exc:
            self._fail_closed_active_bios_evaluator()
            if active_context.returns.has_pointer_captures_after(
                capture_checkpoint
            ):
                active_context._mark_host_control_fault(exc)
            raise
        finally:
            if state is not None:
                compiler = state.compiler
                if compiler is not None and compiler.temporary:
                    state.compiler = None
                    self._discard_temporary_compiler(compiler, state)
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
        """Execute one live word to completion or raise ``ExecutionBlocked``."""

        self._require_session_owner_access("execute a semantic word")
        if self._active_dispatches or self._active_input_states:
            active_context = self.main_context if context is None else context
            if not isinstance(active_context, ExecutionContext):
                raise TypeError("context must be an ExecutionContext")
            self._require_no_suspension("execute a nested semantic dispatch")
            active_context._require_reusable()
            word = self._resolve_word(name_or_xt)
            meter, starting_steps = self._meter_for_public_call(step_budget)
            self._execute_guarded(word, active_context, meter)
            return ExecutionResult(meter.steps - starting_steps)

        result = self.run_until_blocked(
            name_or_xt,
            context=context,
            step_budget=step_budget,
        )
        if isinstance(result, BlockedExecution):
            raise ExecutionBlocked(result.suspension, result.semantic_steps)
        return result

    def run_until_blocked(
        self,
        name_or_xt: bytes | str | int,
        *,
        context: ExecutionContext | None = None,
        step_budget: int | None = None,
    ) -> RunResult:
        """Run one compiled word until completion or its next IDL boundary."""

        with self._session_owner_lock:
            self._require_session_owner_access("run a semantic dispatch")
            try:
                return self._run_until_blocked(
                    name_or_xt,
                    context=context,
                    step_budget=step_budget,
                )
            except BaseException:
                # This entry point rejects nested dispatch before execution, so
                # every exception escaping it is an outer host escape rather than
                # a guest transfer still travelling toward an older CATCH root.
                self._fail_closed_active_bios_evaluator()
                raise

    def _run_until_blocked(
        self,
        name_or_xt: bytes | str | int,
        *,
        context: ExecutionContext | None,
        step_budget: int | None,
    ) -> RunResult:
        """Implement :meth:`run_until_blocked` under its host guard."""

        active_context = self.main_context if context is None else context
        if not isinstance(active_context, ExecutionContext):
            raise TypeError("context must be an ExecutionContext")
        self._require_no_suspension("start another semantic dispatch")
        if self._active_dispatches or self._active_input_states:
            raise ExecutionError(
                "resumable dispatch cannot start inside source evaluation or "
                "another host dispatch"
            )
        active_context._require_reusable()
        word = self._resolve_word(name_or_xt)
        meter, starting_steps = self._meter_for_public_call(step_budget)
        suspended = self._execute_guarded(
            word,
            active_context,
            meter,
            allow_idle=True,
            starting_steps=starting_steps,
        )
        semantic_steps = meter.steps - starting_steps
        if suspended is None:
            return ExecutionResult(semantic_steps)
        return BlockedExecution(semantic_steps, suspended.handle)

    def deliver_idle_wake(
        self,
        suspension: ExecutionSuspension,
        kind: IdleWake,
    ) -> IdleWakeReceipt:
        """Publish one interrupt/DMA wake for an exact blocked dispatch."""

        with self._session_owner_lock:
            self._require_session_owner_access("wake a semantic dispatch")
            return self._deliver_idle_wake_locked(suspension, kind)

    def _deliver_idle_wake_locked(
        self,
        suspension: ExecutionSuspension,
        kind: IdleWake,
    ) -> IdleWakeReceipt:
        blocked = self._require_suspension(suspension)
        if not isinstance(kind, IdleWake):
            raise TypeError("idle wake kind must be an IdleWake")
        if blocked.wake_receipt is not None:
            raise ExecutionError("suspension already has an undelivered wake receipt")
        sequence = self._next_wake_sequence
        self._next_wake_sequence += 1
        if sequence > MASK64:
            raise ExecutionError("idle wake identity space exhausted")
        receipt = IdleWakeReceipt(
            kind,
            sequence,
            suspension.sequence,
            self._runtime_token,
        )
        blocked.wake_receipt = receipt
        return receipt

    def resume(
        self,
        suspension: ExecutionSuspension,
        wake_receipt: IdleWakeReceipt,
    ) -> RunResult:
        """Resume one exact IDL suspension after a runtime-issued wake."""

        with self._session_owner_lock:
            self._require_session_owner_access("resume a semantic dispatch")
            return self._resume_locked(suspension, wake_receipt)

    def _resume_locked(
        self,
        suspension: ExecutionSuspension,
        wake_receipt: IdleWakeReceipt,
    ) -> RunResult:
        blocked = self._require_suspension(suspension)
        if not isinstance(wake_receipt, IdleWakeReceipt):
            raise TypeError("wake receipt must be an IdleWakeReceipt")
        if (
            wake_receipt._runtime_token is not self._runtime_token
            or wake_receipt._suspension_sequence != suspension.sequence
            or blocked.wake_receipt is not wake_receipt
        ):
            raise ExecutionError("wake receipt is stale, foreign, or already consumed")
        if blocked.context.data.snapshot() != blocked.blocked_data_snapshot:
            raise ExecutionError("data stack changed while dispatch was suspended")
        if blocked.context.returns.snapshot() != blocked.blocked_return_snapshot:
            raise ExecutionError("return stack changed while dispatch was suspended")

        blocked.had_pointer_capture = (
            blocked.had_pointer_capture
            or blocked.context.returns.has_pointer_captures_after(
                blocked.capture_checkpoint
            )
        )

        blocked.wake_receipt = None
        self._suspended_execution = None
        old_sequence = suspension.sequence
        blocked.context._release_suspension(old_sequence)
        cursor = self._resume_guarded(blocked)

        semantic_steps = blocked.meter.steps - blocked.starting_steps
        if cursor is None:
            return ExecutionResult(semantic_steps)

        handle: ExecutionSuspension | None = None
        lease_installed = False
        try:
            handle = self._allocate_suspension_handle()
            blocked.handle = handle
            blocked.cursor = cursor
            blocked.blocked_data_snapshot = blocked.context.data.snapshot()
            blocked.blocked_return_snapshot = blocked.context.returns.snapshot()
            blocked.context._lease_for_suspension(handle.sequence)
            lease_installed = True
            self._suspended_execution = blocked
        except BaseException as exc:
            if lease_installed:
                assert handle is not None
                blocked.context._release_suspension(handle.sequence)
            if blocked.had_pointer_capture:
                blocked.context._mark_host_control_fault(exc)
            blocked.context.returns.restore(blocked.return_snapshot)
            blocked.context.returns.restore_pointer_captures(
                blocked.capture_checkpoint
            )
            raise
        assert handle is not None
        return BlockedExecution(semantic_steps, handle)

    def cancel_suspension(self, suspension: ExecutionSuspension) -> None:
        """Unwind internal return state and release one blocked context."""

        with self._session_owner_lock:
            self._require_session_owner_access("cancel a semantic dispatch")
            self._cancel_suspension_locked(suspension)

    def _cancel_suspension_locked(
        self,
        suspension: ExecutionSuspension,
    ) -> None:
        blocked = self._require_suspension(suspension)
        blocked.had_pointer_capture = (
            blocked.had_pointer_capture
            or blocked.context.returns.has_pointer_captures_after(
                blocked.capture_checkpoint
            )
        )
        if blocked.had_pointer_capture:
            blocked.context._mark_host_control_fault(
                ExecutionError("IDL suspension canceled after RP@")
            )
        blocked.context.returns.restore(blocked.return_snapshot)
        blocked.context.returns.restore_pointer_captures(
            blocked.capture_checkpoint
        )
        self._suspended_execution = None
        blocked.context._release_suspension(suspension.sequence)

    def _evaluate_line(self, state: _EvaluationState) -> None:
        self._active_input_states.append(state)
        try:
            while True:
                token = state.cursor.parse_word()
                if not token:
                    return
                state.token_count += 1
                self._evaluate_token(token, state)
                if state.bios_evaluator:
                    evaluator = self._require_bios_evaluator()
                    if self.memory.read64(evaluator.status_address) != 0:
                        return
        finally:
            active = self._active_input_states.pop()
            if active is not state:
                raise AssertionError("active input cursor stack is corrupted")

    def _evaluate_token(self, token: bytes, state: _EvaluationState) -> None:
        if token.startswith(b"\\"):
            state.cursor.skip_backslash_comment()
            return

        try:
            word = self.dictionary.find(token)
        except ValueError:
            if not state.bios_evaluator:
                raise
            # Dictionary header names are ASCII and at most 127 bytes, while
            # the BIOS parser admits any non-space token up to the 255-byte
            # input bound.  Such a token is an ordinary guest lookup miss.
            word = None
        if word is not None and isinstance(word.implementation, DirectiveDefinition):
            if (
                state.compiler is not None
                and state.compiler.compile_mode
                and not word.immediate
            ):
                if word.implementation.kind is DirectiveKind.RIGHT_BRACKET:
                    self._compile_error(
                        state,
                        "] cannot be compiled until persistent STATE is admitted",
                    )
                self._compile_error(
                    state,
                    f"{word.name.decode('ascii')} is a non-executable "
                    "directive and cannot be compiled",
                )
            else:
                self._apply_directive(word.implementation.kind, state)
            return

        if word is not None:
            if (
                state.compiler is not None
                and state.compiler.compile_mode
                and not word.immediate
            ):
                state.compiler.operations.append(Call(word.xt))
            else:
                self._execute_guarded(word, state.context, state.meter)
            return

        number = self._parse_number(token)
        if number is not None:
            if state.compiler is not None and state.compiler.compile_mode:
                state.compiler.operations.append(Literal(number))
            else:
                state.context.data.push(number)
            return

        location = self._token_location(state)
        if state.bios_evaluator:
            raise _UndefinedWord(token, location)
        raise SourceError(f"unknown word {token!r}", location)

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
            if state.compiler is not None and state.compiler.compile_mode:
                state.compiler.operations.append(WriteOutput(payload))
            else:
                self.write_uart_bytes(payload)
            return
        if kind is DirectiveKind.S_QUOTE:
            compiling = (
                state.compiler is not None
                and state.compiler.compile_mode
            )
            if compiling:
                assert state.compiler is not None
                payload = self._parse_quoted_literal(
                    state,
                    unconditionally_skip_next=False,
                )
                if b"\0" in payload:
                    self._compile_error(
                        state,
                        'compiled S" payload contains an embedded NUL',
                    )
                offset = len(state.compiler.literal_pool)
                state.compiler.literal_pool.extend(payload)
                state.compiler.literal_pool.append(0)
                state.compiler.operations.append(
                    PushStringLiteral(offset, len(payload))
                )
            else:
                payload = self._parse_interpreted_s_quote(state)
                state.context.data.require_push_capacity(2)
                self.memory.write_bytes(
                    self._squote_buffer_address,
                    payload + b"\0",
                )
                state.context.data.push_pair(
                    self._squote_buffer_address,
                    len(payload),
                )
            return
        if kind is DirectiveKind.ABORT_QUOTE:
            if state.compiler is None or not state.compiler.compile_mode:
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

        if kind is DirectiveKind.LEFT_BRACKET:
            if state.compiler is not None:
                state.compiler.compile_mode = False
            return
        if kind is DirectiveKind.RIGHT_BRACKET:
            if state.compiler is None:
                self._compile_error(state, "] requires an open definition")
            state.compiler.compile_mode = True
            return

        compiler = state.compiler
        if kind is DirectiveKind.IF and compiler is None:
            compiler = _Compiler(
                b"<interpret-if>",
                self._token_location(state),
                temporary_checkpoint=self.dictionary.checkpoint(),
            )
            state.compiler = compiler
        if compiler is None or not compiler.compile_mode:
            self._compile_error(state, f"{kind.name} is compile-only")

        if kind is DirectiveKind.SEMICOLON:
            if compiler.temporary:
                self._compile_error(
                    state,
                    "; cannot terminate a temporary interpret IF",
                )
            if compiler.controls:
                self._compile_error(state, "; has unresolved control flow")
            operations = tuple((*compiler.operations, Return()))
            word = self._define_dictionary_word(
                compiler.name,
                ColonDefinition(operations),
                initial_body=bytes(compiler.literal_pool),
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
            if compiler.temporary and not compiler.controls:
                self._execute_temporary_compiler(compiler, state)
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
        elif kind is DirectiveKind.TWO_TO_R:
            compiler.operations.append(RPushPair())
        elif kind is DirectiveKind.TWO_R_FROM:
            compiler.operations.append(RPopPair())
        elif kind is DirectiveKind.TWO_R_FETCH:
            compiler.operations.append(RPeekPair())
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

    def _execute_temporary_compiler(
        self,
        compiler: _Compiler,
        state: _EvaluationState,
    ) -> None:
        """Execute and erase one outer interpret-mode ``IF`` compilation.

        Native BIOS compiles an ordinary interpret-time ``IF`` at the current
        ``HERE``, switches back to interpretation at the outer ``THEN``, calls
        that anonymous code, clears its bytes, and restores ``HERE``.  Hosted
        IR needs no published dictionary header, but it still materializes a
        private code slot and literal pool so ``S"`` addresses and nested
        colon continuations have the same lifetime boundary.
        """

        if state.compiler is not compiler or not compiler.temporary:
            raise AssertionError("temporary compiler is not the active compiler")
        if compiler.controls:
            raise AssertionError("temporary compiler still has open controls")

        checkpoint = compiler.temporary_checkpoint
        assert checkpoint is not None
        state.compiler = None
        scratch_start = self.dictionary.here
        scratch_limit = scratch_start
        transient: Word | None = None
        identity_corrupted = False
        try:
            if scratch_start in self._transient_words:
                raise ExecutionError(
                    "temporary interpret IF overlaps an active anonymous word"
                )
            try:
                self.dictionary.resolve(scratch_start)
            except KeyError:
                pass
            else:
                raise ExecutionError(
                    "temporary interpret IF overlaps a live execution token"
                )

            literal_pool = bytes(compiler.literal_pool)
            width = CELL_BYTES + len(literal_pool)
            self._preflight_dictionary_growth(width, state.context)
            self.dictionary.allot(width)
            scratch_limit = scratch_start + width
            self.memory.fill(scratch_start, width, 0)
            if literal_pool:
                self.memory.write_bytes(scratch_start + CELL_BYTES, literal_pool)

            transient = Word(
                name=b"<interpret-if>",
                header_address=scratch_start,
                xt=scratch_start,
                immediate=False,
                implementation=ColonDefinition(
                    tuple((*compiler.operations, Return()))
                ),
            )
            self._transient_words[scratch_start] = transient
            self._execute_guarded(
                transient,
                state.context,
                state.meter,
            )
        finally:
            if transient is not None:
                removed = self._transient_words.pop(transient.xt, None)
                if removed is not transient:
                    identity_corrupted = True
            self._discard_temporary_compiler(
                compiler,
                state,
                clear_start=scratch_start,
                clear_limit=scratch_limit,
            )
            if identity_corrupted:
                raise AssertionError(
                    "temporary interpret IF identity was corrupted"
                )

    def _discard_temporary_compiler(
        self,
        compiler: _Compiler,
        state: _EvaluationState | None = None,
        *,
        clear_start: int | None = None,
        clear_limit: int | None = None,
    ) -> None:
        """Roll back one anonymous compiler and optionally erase its bytes."""

        checkpoint = compiler.temporary_checkpoint
        if checkpoint is None:
            return

        observed_here = self.dictionary.here
        erase_start = clear_start
        erase_limit = clear_limit
        if erase_start is not None and erase_limit is not None:
            candidate_limit = max(erase_limit, observed_here)
            for region in self.memory.regions:
                if (
                    region.base <= checkpoint.here
                    and candidate_limit <= region.limit
                ):
                    erase_start = checkpoint.here
                    erase_limit = candidate_limit
                    break

        self.dictionary.rollback(checkpoint)
        self.dictionary_index.rebuild()
        compiler.temporary_checkpoint = None
        if (
            erase_start is not None
            and erase_limit is not None
            and erase_limit > erase_start
        ):
            self.memory.fill(erase_start, erase_limit - erase_start, 0)
        if state is not None and state.definitions:
            # BIOS gives every physical EVALUATE a fresh result ledger while
            # its compiler persists across calls.  Retain entries by live
            # word identity after rollback instead of applying an opening
            # call's list offset to the closing call's unrelated ledger.
            live_by_xt = {word.xt: word for word in self.dictionary.words}
            state.definitions[:] = [
                word
                for word in state.definitions
                if live_by_xt.get(word.xt) is word
            ]

    def _execute_guarded(
        self,
        word: Word,
        context: ExecutionContext,
        meter: _StepMeter,
        *,
        allow_idle: bool = False,
        starting_steps: int = 0,
    ) -> _SuspendedExecution | None:
        """Execute atomically with respect to internal return-stack state."""

        context._require_reusable()
        return_snapshot = context.returns.snapshot()
        capture_checkpoint = context.returns.pointer_capture_checkpoint()
        root_id = self._allocate_dispatch_root_id()
        frame = _DispatchFrame(context, meter, root_id)
        preserve_capture_evidence = False
        completed_successfully = False
        suspended: _SuspendedExecution | None = None
        self._active_dispatches.append(frame)
        try:
            cursor = self._execute_top(
                word,
                context,
                meter,
                root_id=root_id,
                allow_idle=allow_idle,
            )
            if cursor is None:
                completed_successfully = True
            else:
                handle = self._allocate_suspension_handle()
                suspended = _SuspendedExecution(
                    handle=handle,
                    context=context,
                    meter=meter,
                    starting_steps=starting_steps,
                    root_id=root_id,
                    cursor=cursor,
                    return_snapshot=return_snapshot,
                    capture_checkpoint=capture_checkpoint,
                    had_pointer_capture=(
                        context.returns.has_pointer_captures_after(
                            capture_checkpoint
                        )
                    ),
                    blocked_data_snapshot=context.data.snapshot(),
                    blocked_return_snapshot=context.returns.snapshot(),
                )
                context._lease_for_suspension(handle.sequence)
                self._suspended_execution = suspended
                preserve_capture_evidence = True
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
            self._fail_closed_active_bios_evaluator()
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
            self._fail_closed_active_bios_evaluator()
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
        return suspended

    def _resume_guarded(
        self,
        suspended: _SuspendedExecution,
    ) -> _DispatchCursor | None:
        """Continue a detached dispatch under its original host guard."""

        context = suspended.context
        resume_capture_checkpoint = (
            context.returns.pointer_capture_checkpoint()
        )
        frame = _DispatchFrame(context, suspended.meter, suspended.root_id)
        preserve_capture_evidence = False
        completed_successfully = False
        cursor: _DispatchCursor | None = None
        self._active_dispatches.append(frame)
        try:
            cursor = self._execute_top(
                None,
                context,
                suspended.meter,
                root_id=suspended.root_id,
                resume_cursor=suspended.cursor,
                allow_idle=True,
            )
            suspended.had_pointer_capture = (
                suspended.had_pointer_capture
                or context.returns.has_pointer_captures_after(
                    resume_capture_checkpoint
                )
            )
            completed_successfully = cursor is None
            if cursor is not None:
                preserve_capture_evidence = True
        except _GuestControlTransfer as transfer:
            suspended.had_pointer_capture = (
                suspended.had_pointer_capture
                or context.returns.has_pointer_captures_after(
                    resume_capture_checkpoint
                )
            )
            if transfer.context is not context:
                if suspended.had_pointer_capture:
                    context._mark_host_control_fault(transfer)
                context.returns.restore(suspended.return_snapshot)
                raise
            if transfer.root_id != suspended.root_id:
                if suspended.had_pointer_capture:
                    context._mark_host_control_fault(transfer)
                preserve_capture_evidence = True
                raise
            completed_successfully = True
            cursor = None
        except _DictionaryFaultRequest as request:
            suspended.had_pointer_capture = (
                suspended.had_pointer_capture
                or context.returns.has_pointer_captures_after(
                    resume_capture_checkpoint
                )
            )
            if request.context is not context:
                if suspended.had_pointer_capture:
                    context._mark_host_control_fault(request)
                context.returns.restore(suspended.return_snapshot)
                raise
            context.returns.restore(suspended.return_snapshot)
            if suspended.had_pointer_capture:
                context._mark_host_control_fault(request)
            preserve_capture_evidence = True
            raise
        except ForthAbort as exc:
            self._fail_closed_active_bios_evaluator()
            suspended.had_pointer_capture = (
                suspended.had_pointer_capture
                or context.returns.has_pointer_captures_after(
                    resume_capture_checkpoint
                )
            )
            if suspended.had_pointer_capture:
                context._mark_host_control_fault(exc)
            if exc.bind_origin(context):
                context.data.clear()
                context.returns.clear()
            else:
                context.returns.restore(suspended.return_snapshot)
            raise
        except BaseException as exc:
            self._fail_closed_active_bios_evaluator()
            suspended.had_pointer_capture = (
                suspended.had_pointer_capture
                or context.returns.has_pointer_captures_after(
                    resume_capture_checkpoint
                )
            )
            if suspended.had_pointer_capture:
                context._mark_host_control_fault(exc)
            context.returns.restore(suspended.return_snapshot)
            raise
        finally:
            if completed_successfully and (
                self._has_older_dispatch(context)
                or self._has_active_evaluation(context)
            ):
                preserve_capture_evidence = True
            if not preserve_capture_evidence:
                context.returns.restore_pointer_captures(
                    suspended.capture_checkpoint
                )
            active = self._active_dispatches.pop()
            if active is not frame:
                raise AssertionError("active semantic dispatch stack is corrupted")
        return cursor

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
            self._fail_closed_active_bios_evaluator()
            if context.returns.has_pointer_captures_after(capture_checkpoint):
                context._mark_host_control_fault(exc)
            if exc.bind_origin(context):
                context.data.clear()
                context.returns.clear()
            else:
                context.returns.restore(return_snapshot)
            raise
        except BaseException as exc:
            self._fail_closed_active_bios_evaluator()
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
            target = self._resolve_dispatch_word(request.xt)
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
            target = self._resolve_dispatch_word(invocation.xt)

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
        resume_cursor: _DispatchCursor | None = None,
        allow_idle: bool = False,
    ) -> _DispatchCursor | None:
        fault_entry = fault_request is not None
        if resume_cursor is not None:
            if word is not None or fault_request is not None:
                raise AssertionError("resumed dispatch cannot have a new entry target")
            current = self._resolve_dispatch_word(resume_cursor.xt)
            if not isinstance(current.implementation, ColonDefinition):
                raise ExecutionError("suspended definition is no longer executable")
            ip = resume_cursor.ip
        else:
            current = None
            ip = 0
        if resume_cursor is None and fault_request is not None:
            target, entry_ip = self._begin_dictionary_fault(
                fault_request,
                context,
                meter,
            )
        elif resume_cursor is None:
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
                target = self._resolve_dispatch_word(invocation.xt)

        if resume_cursor is None:
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

        assert current is not None

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
                called = self._resolve_dispatch_word(operation.xt)
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
            elif isinstance(operation, Idle):
                if not allow_idle:
                    raise ExecutionError(
                        "IDL cannot suspend source evaluation or a nested host "
                        "dispatch; use run_until_blocked on a compiled word"
                    )
                return _DispatchCursor(current.xt, ip + 1)
            elif isinstance(operation, UartReadAttempt):
                # Native KEY flushes its buffered TX stream before polling.
                # Hosted UART output is published immediately, so that flush
                # has no stateful counterpart here.
                value = self._take_uart_input_byte()
                if value is None:
                    context.data.push(0)
                else:
                    context.data.push(value)
                    context.data.push(MASK64)
                ip += 1
            elif isinstance(operation, RPush):
                context.returns.push(context.data.pop())
                ip += 1
            elif isinstance(operation, RPop):
                context.data.push(context.returns.pop())
                ip += 1
            elif isinstance(operation, RPeek):
                context.data.push(context.returns.peek())
                ip += 1
            elif isinstance(operation, RPushPair):
                # Preflight both stacks before consuming either source cell.
                context.returns.require_push_capacity(2)
                first, second = context.data.pop_pair("2>R")
                context.returns.push_pair(first, second)
                ip += 1
            elif isinstance(operation, RPopPair):
                # Shape and destination checks make the cross-stack transfer
                # fail without partially consuming the ordered pair.
                first, second = context.returns.peek_pair("2R>")
                context.data.require_push_capacity(2)
                context.returns.pop_pair("2R>")
                context.data.push_pair(first, second)
                ip += 1
            elif isinstance(operation, RPeekPair):
                first, second = context.returns.peek_pair("2R@")
                context.data.require_push_capacity(2)
                context.data.push_pair(first, second)
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
            elif isinstance(operation, PushStringLiteral):
                context.data.push_pair(
                    current.body_address + operation.offset,
                    operation.length,
                )
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
                caller = self._resolve_dispatch_word(continuation.xt)
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
            target = self._resolve_dispatch_word(invocation.xt)

        if not isinstance(target.implementation, ColonDefinition):
            raise ExecutionError(f"word {target.name!r} is not executable")
        context.returns.push_continuation(caller.xt, return_ip)
        return target, 0

    def _resolve_does_entry(self, action: DoesBodyRef) -> tuple[Word, int]:
        source = self._resolve_dispatch_word(action.source_xt)
        if not isinstance(source.implementation, ColonDefinition):
            raise ExecutionError("DOES> action does not name a colon definition")
        return source, action.entry_ip

    def _resolve_word(self, name_or_xt: bytes | str | int) -> Word:
        if isinstance(name_or_xt, int):
            return self._resolve_dispatch_word(name_or_xt)
        word = self.dictionary.find(name_or_xt)
        if word is None:
            raise KeyError(f"unknown word {name_or_xt!r}")
        return word

    def _resolve_dispatch_word(self, xt: int) -> Word:
        """Resolve a live word, including an active anonymous IF body."""

        transient = self._transient_words.get(xt)
        if transient is not None:
            return transient
        return self.dictionary.resolve(xt)

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
        meter = _StepMeter(step_budget, self._account_semantic_step)
        return meter, 0

    def _account_semantic_step(self) -> None:
        """Advance each runtime-local work clock for one admitted step."""

        self.diagnostics.account_work()
        self.timer.advance()

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

    @staticmethod
    def _parse_interpreted_s_quote(state: _EvaluationState) -> bytes:
        """Consume the native one-byte delimiter and bounded transient text."""

        state.cursor.consume_byte()
        payload = bytearray()
        while len(payload) < _BIOS_SQUOTE_MAX_PAYLOAD:
            value = state.cursor.consume_byte()
            if value is None or value == ord('"'):
                break
            payload.append(value)
        return bytes(payload)

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
    "BlockedExecution",
    "ColonDefinition",
    "ConstantDefinition",
    "CreatedDefinition",
    "DirectiveDefinition",
    "DirectiveKind",
    "DoesBodyRef",
    "EvaluationResult",
    "ExecutionContext",
    "ExecutionResult",
    "ExecutionSuspension",
    "IdleWake",
    "IdleWakeReceipt",
    "Invoke",
    "MegaForthRuntime",
    "PrimitiveCallback",
    "PrimitiveDefinition",
    "RunResult",
    "WordImplementation",
]
