"""Explicit hosted MegaForth compiler and semantic dispatch loop.

The first runtime slice deliberately implements a small real kernel rather
than translating source into Python calls.  Colon continuations remain on the
same ordered return stack as user ``>R`` cells and counted-loop state, and
compiled calls retain the execution token that was bound at compile time.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, TypeAlias

from shared.cells import CELL_BYTES, u64
from simulator.dictionary import Dictionary, Word
from simulator.errors import (
    ExecutionError,
    ForthAbort,
    SourceError,
    StepBudgetExceeded,
)
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
from simulator.memory import AddressClass, SparseAddressSpace
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
    def __init__(self, budget: int | None) -> None:
        if budget is not None:
            if not isinstance(budget, int):
                raise TypeError("step budget must be an integer or None")
            if budget < 1:
                raise ValueError("step budget must be positive")
        self.budget = budget
        self.steps = 0

    def tick(self) -> None:
        if self.budget is not None and self.steps >= self.budget:
            raise StepBudgetExceeded(self.budget)
        self.steps += 1


class MegaForthRuntime:
    """Hosted dictionary, evaluator, and explicit semantic dispatcher."""

    def __init__(
        self,
        *,
        dictionary_start: int = 0x1000,
        memory: SparseAddressSpace | None = None,
        install_core_words: bool = True,
    ) -> None:
        if memory is not None and not isinstance(memory, SparseAddressSpace):
            raise TypeError("memory must be a SparseAddressSpace or None")
        if memory is None:
            from simulator.platform import create_one_core_address_space

            self.memory = create_one_core_address_space()
        else:
            self.memory = memory
        self.dictionary = Dictionary(
            start_address=dictionary_start,
            memory=self.memory,
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
        self._numeric_base = 10
        # These are the source-visible optional user-dictionary bounds, not
        # Dictionary's ordinary containing-memory ceiling.  Bank-0 source
        # starts with the BIOS interval disabled, represented by two zeros.
        self._dictionary_base = 0
        self._dictionary_limit = 0
        self._provided: set[bytes] = set()
        self._active_input_states: list[_EvaluationState] = []
        self._active_meters: list[_StepMeter] = []
        self._uart_output = bytearray()
        if install_core_words:
            from simulator.core_words import install_core

            install_core(self)

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

        return self._numeric_base

    @property
    def dictionary_base(self) -> int:
        """Return the active BIOS user-dictionary base, or zero when off."""

        return self._dictionary_base

    @property
    def dictionary_limit(self) -> int:
        """Return the active BIOS user-dictionary limit, or zero when off."""

        return self._dictionary_limit

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
        contents.  Until hosted memory installs that address, this method is
        the semantic backing used by ``HEX`` and ``DECIMAL``.
        """

        self._numeric_base = u64(base)

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
        return self.dictionary.define(
            name,
            PrimitiveDefinition(callback),
            immediate=immediate,
        )

    def define_constant(self, name: bytes | str, value: int) -> Word:
        """Publish an executable cell constant under one stable XT.

        Constants created by a defining primitive during :meth:`evaluate` are
        also included in that evaluation's ordered definition ledger.
        """

        word = self.dictionary.define(name, ConstantDefinition(value))
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

        word = self.dictionary.define(
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
        return self.dictionary.define(
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
        address = self.dictionary.write_transient(counted)
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
        state: _EvaluationState | None = None
        line_count = 0
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
            state.compiler = _Compiler(name, self._token_location(state))
            return

        compiler = state.compiler
        if compiler is None:
            self._compile_error(state, f"{kind.name} is compile-only")

        if kind is DirectiveKind.SEMICOLON:
            if compiler.controls:
                self._compile_error(state, "; has unresolved control flow")
            compiler.operations.append(Return())
            word = self.dictionary.define(
                compiler.name,
                ColonDefinition(tuple(compiler.operations)),
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
            if frame.question_index is not None:
                compiler.operations[frame.question_index] = QuestionDo(
                    len(compiler.operations)
                )
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
        self._active_meters.append(meter)
        try:
            self._execute_top(word, context, meter)
        except ForthAbort as exc:
            # ABORT deliberately cleared the complete task.  Restoring the
            # pre-dispatch return stack would resurrect caller continuations.
            if context.returns.has_pointer_captures_after(capture_checkpoint):
                context._mark_host_control_fault(exc)
            raise
        except BaseException as exc:
            if context.returns.has_pointer_captures_after(capture_checkpoint):
                context._mark_host_control_fault(exc)
            context.returns.restore(return_snapshot)
            raise
        finally:
            context.returns.restore_pointer_captures(capture_checkpoint)
            active = self._active_meters.pop()
            if active is not meter:
                raise AssertionError("active semantic step-meter stack is corrupted")

    def _execute_top(
        self,
        word: Word,
        context: ExecutionContext,
        meter: _StepMeter,
    ) -> None:
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
                target, entry_ip = self._resolve_does_entry(implementation.action)
                break
            if not isinstance(implementation, PrimitiveDefinition):
                break
            meter.tick()
            invocation = implementation.callback(context)
            if invocation is None:
                return
            if not isinstance(invocation, Invoke):
                raise ExecutionError("primitive returned an invalid control result")
            target = self.dictionary.resolve(invocation.xt)

        if not isinstance(target.implementation, ColonDefinition):
            raise ExecutionError(f"word {target.name!r} is not executable")
        context.returns.push_continuation(target.xt, entry_ip, root=True)
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
                    raise ForthAbort('Forth ABORT"')
                ip += 1
            elif isinstance(operation, Return):
                continuation = context.returns.pop_continuation()
                if continuation.root:
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
            invocation = implementation.callback(context)
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

        if self._active_meters:
            if step_budget is not None:
                raise ValueError(
                    "nested evaluation or execution cannot replace the active "
                    "step budget"
                )
            meter = self._active_meters[-1]
            return meter, meter.steps
        meter = _StepMeter(step_budget)
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

        base = self._numeric_base
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
