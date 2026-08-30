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

from shared.cells import u64
from simulator.dictionary import Dictionary, Word
from simulator.errors import ExecutionError, SourceError, StepBudgetExceeded
from simulator.ir import (
    Branch,
    BranchZero,
    Call,
    Do,
    Literal,
    Loop,
    Operation,
    RPeek,
    RPop,
    RPush,
    Return,
    Unloop,
)
from simulator.source import SourceBuffer, SourceCursor, SourceLocation
from simulator.stacks import DataStack, ReturnStack


@dataclass(slots=True)
class ExecutionContext:
    """Per-task semantic stacks for one hosted execution context."""

    data: DataStack = field(default_factory=DataStack)
    returns: ReturnStack = field(default_factory=ReturnStack)


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


class DirectiveKind(Enum):
    COLON = auto()
    SEMICOLON = auto()
    IF = auto()
    ELSE = auto()
    THEN = auto()
    EXIT = auto()
    TO_R = auto()
    R_FROM = auto()
    R_FETCH = auto()
    DO = auto()
    LOOP = auto()
    UNLOOP = auto()
    PAREN_COMMENT = auto()
    BACKSLASH_COMMENT = auto()
    PROVIDED = auto()


@dataclass(frozen=True, slots=True)
class DirectiveDefinition:
    kind: DirectiveKind


WordImplementation: TypeAlias = (
    PrimitiveDefinition | ColonDefinition | DirectiveDefinition
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


_ControlFrame: TypeAlias = _IfFrame | _DoFrame


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
        install_core_words: bool = True,
    ) -> None:
        self.dictionary = Dictionary(start_address=dictionary_start)
        self.main_context = ExecutionContext()
        self._numeric_base = 10
        self._provided: set[bytes] = set()
        if install_core_words:
            from simulator.core_words import install_core

            install_core(self)

    @property
    def provided_modules(self) -> frozenset[bytes]:
        return frozenset(self._provided)

    @property
    def numeric_base(self) -> int:
        """Current unsigned cell used by the BIOS-compatible number parser."""

        return self._numeric_base

    def set_numeric_base(self, base: int) -> None:
        """Set the source number base as a wrapped guest cell.

        The BIOS exposes ``BASE`` as a mutable cell and does not constrain its
        contents.  Until hosted memory installs that address, this method is
        the semantic backing used by ``HEX`` and ``DECIMAL``.
        """

        self._numeric_base = u64(base)

    def new_context(self) -> ExecutionContext:
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

    def define_directive(self, name: bytes | str, kind: DirectiveKind) -> Word:
        if not isinstance(kind, DirectiveKind):
            raise TypeError("directive kind must be a DirectiveKind")
        return self.dictionary.define(
            name,
            DirectiveDefinition(kind),
            immediate=True,
        )

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

        meter = _StepMeter(step_budget)
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
            semantic_steps=meter.steps,
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
        word = self._resolve_word(name_or_xt)
        meter = _StepMeter(step_budget)
        self._execute_guarded(word, active_context, meter)
        return ExecutionResult(meter.steps)

    def _evaluate_line(self, state: _EvaluationState) -> None:
        while True:
            token = state.cursor.parse_word()
            if not token:
                return
            state.token_count += 1
            self._evaluate_token(token, state)

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
        elif kind is DirectiveKind.LOOP:
            if not compiler.controls or not isinstance(
                compiler.controls[-1], _DoFrame
            ):
                self._compile_error(state, "LOOP has no matching DO")
            frame = compiler.controls.pop()
            assert isinstance(frame, _DoFrame)
            compiler.operations.append(Loop(frame.body_target))
        elif kind is DirectiveKind.UNLOOP:
            compiler.operations.append(Unloop())
        else:
            raise AssertionError(f"unhandled directive {kind}")

    def _execute_guarded(
        self,
        word: Word,
        context: ExecutionContext,
        meter: _StepMeter,
    ) -> None:
        """Execute atomically with respect to internal return-stack state."""

        return_snapshot = context.returns.snapshot()
        try:
            self._execute_top(word, context, meter)
        except BaseException:
            context.returns.restore(return_snapshot)
            raise

    def _execute_top(
        self,
        word: Word,
        context: ExecutionContext,
        meter: _StepMeter,
    ) -> None:
        target = word
        while isinstance(target.implementation, PrimitiveDefinition):
            meter.tick()
            invocation = target.implementation.callback(context)
            if invocation is None:
                return
            if not isinstance(invocation, Invoke):
                raise ExecutionError("primitive returned an invalid control result")
            target = self.dictionary.resolve(invocation.xt)

        if not isinstance(target.implementation, ColonDefinition):
            raise ExecutionError(f"word {target.name!r} is not executable")

        context.returns.push_continuation(target.xt, 0, root=True)
        current = target
        ip = 0

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
                    current = entered
                    ip = 0
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
            elif isinstance(operation, Loop):
                ip = operation.target if context.returns.loop() else ip + 1
            elif isinstance(operation, Unloop):
                context.returns.unloop()
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
    ) -> Word | None:
        while isinstance(target.implementation, PrimitiveDefinition):
            meter.tick()
            invocation = target.implementation.callback(context)
            if invocation is None:
                return None
            if not isinstance(invocation, Invoke):
                raise ExecutionError("primitive returned an invalid control result")
            target = self.dictionary.resolve(invocation.xt)

        if not isinstance(target.implementation, ColonDefinition):
            raise ExecutionError(f"word {target.name!r} is not executable")
        context.returns.push_continuation(caller.xt, return_ip)
        return target

    def _resolve_word(self, name_or_xt: bytes | str | int) -> Word:
        if isinstance(name_or_xt, int):
            return self.dictionary.resolve(name_or_xt)
        word = self.dictionary.find(name_or_xt)
        if word is None:
            raise KeyError(f"unknown word {name_or_xt!r}")
        return word

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

    def _compile_error(self, state: _EvaluationState, message: str) -> None:
        raise SourceError(message, self._token_location(state))


__all__ = [
    "ColonDefinition",
    "DirectiveDefinition",
    "DirectiveKind",
    "EvaluationResult",
    "ExecutionContext",
    "ExecutionResult",
    "Invoke",
    "MegaForthRuntime",
    "PrimitiveCallback",
    "PrimitiveDefinition",
    "WordImplementation",
]
