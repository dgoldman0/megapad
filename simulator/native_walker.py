"""Drive a KDOS loader overlay's line loop, and ``TOKEN-REPLAY``'s record
lines, through the native line walker.

``_megaforth_native.NativeLineWalker`` runs the loop of the
``SOURCE-EVALUATE-CHECKED`` or ``_LD-WALK`` overlay.  It evaluates each line
the checked-line rule admits, with every guest-visible effect of the Python
fast path, and stops at a line it does not admit so the overlay can evaluate
that line on its ordinary path.  For ``TOKEN-REPLAY`` it evaluates the record
lines of a token stream in the same way and stops at one it cannot take, so
that the runtime's replay loop runs that line.  While a token recorder is
attached the walker also reports what it did with the tokens of each source
line it evaluated, and those reports are passed to the recorder here.  See
``docs/simulator-native-evaluator.md``.

The open definition crosses this boundary in both directions: the walker
takes the evaluator's compiler when it starts and gives back the one it holds
when it stops, so exactly one side owns it at any time.
"""

from __future__ import annotations

from typing import Any

from simulator.diagnostics import HostedDiagnosticsService
from simulator.dictionary import Word
from simulator.native_dictionary import (
    NativeDictionary,
    operation_from_native,
    operation_to_native,
)
from simulator.runtime import (
    DirectiveKind,
    ExecutionContext,
    MegaForthRuntime,
    _BeginFrame,
    _CaseFrame,
    _Compiler,
    _DoFrame,
    _IfFrame,
    _OfFrame,
    _StepMeter,
    _WhileFrame,
)
from simulator.source import SourceLocation
from simulator.source_acceleration import WALK_DONE, WALK_ITERATION
from simulator.stacks import DataStack, ReturnStack
from simulator.timer import HostedTimerService


_FRAME_IF, _FRAME_DO, _FRAME_BEGIN, _FRAME_WHILE, _FRAME_CASE, _FRAME_OF = range(6)


class NativeSourceWalker:
    """One overlay's binding to the native line walker."""

    def __init__(
        self,
        runtime: MegaForthRuntime,
        *,
        loader: bool,
        cells: tuple[int, int, int, int, int, int],
        evaluate_checked: Word,
        status_throw: Word | None,
        interpret_primitives: frozenset[Word],
        defining_primitives: dict[Word, int],
    ) -> None:
        dictionary = runtime.dictionary
        if not isinstance(dictionary, NativeDictionary):
            raise TypeError("the native line walker needs the native dictionary")
        native = dictionary.native
        import _megaforth_native as extension

        evaluator = runtime._require_bios_evaluator()
        bank0 = runtime._bank0
        self._runtime = runtime
        self._evaluator = evaluator
        self._evaluate_checked = evaluate_checked
        self._status_throw = status_throw
        self._extension = extension
        # The recorder numbers a line by the evaluator's line cell, which
        # the tap reads as the EVAL-LINE this overlay writes.
        self._tappable = cells[5] == evaluator.line_address
        self._native = extension.NativeLineWalker(
            native,
            # The walker compiles named definitions only.  It is not told of
            # :NONAME, so it hands a line that holds one to Python.
            {
                kind.name: kind.value
                for kind in DirectiveKind
                if kind is not DirectiveKind.NONAME
            },
            [
                (word.xt, word.name.decode("ascii"))
                for word in (*interpret_primitives, *defining_primitives)
            ],
            extension.WALK_LOADER if loader else extension.WALK_SOURCE,
            cells,
            (
                evaluator.status_address,
                evaluator.column_address,
                evaluator.depth_address,
                evaluator.throw_address,
            ),
            evaluate_checked.xt,
            0 if status_throw is None else status_throw.xt,
            runtime._bootstrap_numeric_base,
            bank0.base,
            bank0.limit,
        )

    def walk(self, context: ExecutionContext) -> Any:
        """Run lines natively until one needs Python.

        Returns ``WALK_DONE`` when the overlay's loop has ended,
        ``WALK_ITERATION`` when Python must run the next whole iteration, or
        ``(current, evaluated)`` for a line that is measured, with its cells
        written, and still to be evaluated.
        """

        runtime = self._runtime
        evaluator = self._evaluator
        compiler_state = evaluator.compiler_state
        compiler = compiler_state.compiler
        meter = runtime._active_dispatches[-1].meter
        blocked = not self._admits(context, meter, compiler_state)
        recorder = runtime.token_recorder
        tap = False
        if recorder is not None and not blocked:
            # A recording boot.  The walker reports the lines it evaluates;
            # where it may not, every line goes to the reference path, whose
            # own taps record it.
            tap = recorder.native_tap and self._tappable
            blocked = not tap
        data = context.data
        returns = context.returns
        exported = None if blocked or compiler is None else _export_compiler(compiler)
        allowance = -1 if meter.budget is None else meter.budget - meter.steps
        try:
            result = self._native.walk(
                (data._floor, data._empty_pointer, data._pointer),
                (
                    returns._floor,
                    returns._empty_pointer,
                    returns._pointer,
                    returns._continuation_cookie,
                ),
                len(evaluator.frames),
                exported,
                blocked,
                allowance,
                runtime._dictionary_base,
                runtime._dictionary_limit,
                runtime._numeric_base_address,
                tap,
            )
        except BaseException:
            state = self._native.take_error_state()
            if state is not None:
                self._settle(state, context, meter, blocked)
            raise
        return self._settle(result, context, meter, blocked)

    def stats(self) -> tuple[int, int]:
        """Lines evaluated natively and lines handed to Python so far."""

        return self._native.stats()

    def _admits(self, context, meter, compiler_state) -> bool:
        """Whether native lines can reproduce the fast path at all now."""

        runtime = self._runtime
        overlays = runtime._colon_accelerators
        checked = overlays.get(self._evaluate_checked.xt)
        if checked is None or checked.word is not self._evaluate_checked:
            return False
        if self._status_throw is not None:
            translator = overlays.get(self._status_throw.xt)
            if translator is None or translator.word is not self._status_throw:
                return False
        return _state_admits(runtime, context, meter, compiler_state)

    def _settle(self, result, context, meter, blocked: bool) -> Any:
        (
            outcome, current, evaluated, data_pointer, return_pointer, cookie,
            steps, compiler, cleared_token, tapped,
        ) = result
        context.data._pointer = data_pointer
        context.returns._pointer = return_pointer
        context.returns._continuation_cookie = cookie
        if steps:
            # Batched exactly as after a native execution interval.
            meter.steps += steps
            self._runtime.diagnostics.account_work_many(steps)
            self._runtime.timer.advance_by(steps)
        if cleared_token:
            self._evaluator.token_length = 0
        if not blocked:
            self._evaluator.compiler_state.compiler = _import_compiler(compiler)
        if tapped:
            # Before Python runs the line handed back, so that the recorder
            # hears of every line in the order the file has them.
            self._report(tapped)
        if outcome == 0:
            return WALK_DONE
        if outcome == 2:
            return WALK_ITERATION
        return current, evaluated

    def _report(self, lines: list) -> None:
        """Give the token recorder the lines a walk evaluated, in order."""

        extension = self._extension
        tap_word = extension.TAP_WORD
        tap_number = extension.TAP_NUMBER
        recorder = self._runtime.token_recorder
        depth = len(self._evaluator.frames)
        for address, length, number, events in lines:
            unit = recorder.line(address, length, depth, number)
            if unit is None:
                continue
            for event in events:
                kind = event[0]
                if kind == tap_word:
                    unit.word(event[1], event[2], event[3], event[4])
                elif kind == tap_number:
                    unit.found_number(event[1])
                else:
                    unit.defined(event[1])


def _state_admits(runtime, context, meter, compiler_state) -> bool:
    """Whether the walker can take lines in the evaluator's present state:
    the stacks, clocks and meter are the ones its settlement assumes, and no
    definition or skip is open that it does not hold."""

    compiler = compiler_state.compiler
    return (
        compiler_state.conditional_skip is None
        and (
            compiler is None
            or not (compiler.temporary or compiler.anonymous)
        )
        and runtime._suspended_execution is None
        and type(context.data) is DataStack
        and type(context.returns) is ReturnStack
        and context.data._memory is runtime.memory
        and context.returns._memory is runtime.memory
        and type(runtime.diagnostics) is HostedDiagnosticsService
        and type(runtime.timer) is HostedTimerService
        and type(meter) is _StepMeter
        and getattr(meter._on_tick, "__self__", None) is runtime
        and getattr(meter._on_tick, "__func__", None)
        is MegaForthRuntime._account_semantic_step
    )


class _NativeSlots:
    """The words a native token stream's slots are bound to, as the list the
    runtime's replay loop keeps.

    The native stream holds the bindings.  A word Python bound is also kept
    here as it was found.  A word the walker bound is looked up by its XT
    when Python first uses its slot: it is still the live definition then,
    because the walker binds no slot while a temporary definition is open,
    and every other removal from the dictionary ends the replay.
    """

    __slots__ = ("native", "_walker", "_dictionary", "_words")

    def __init__(self, walker, native, dictionary) -> None:
        self.native = native
        self._walker = walker
        self._dictionary = dictionary
        self._words: dict[int, Word] = {}

    def __len__(self) -> int:
        return self.native.slot_count()

    def __getitem__(self, slot: int) -> Word:
        word = self._words.get(slot)
        if word is None:
            word = self._dictionary.live_word(self.native.slot_xt(slot))
            if word is None:
                raise AssertionError("a token stream's slot lost its word")
            self._words[slot] = word
        return word

    def append(self, word: Word) -> None:
        self._words[self.native.slot_count()] = word
        self._walker.bind_slot(self.native, word.xt)


class NativeTokenWalker:
    """A runtime's binding of ``TOKEN-REPLAY`` to the native line walker."""

    def __init__(self, runtime: MegaForthRuntime) -> None:
        from simulator.source_acceleration import _checked_line_primitives

        dictionary = runtime.dictionary
        if not isinstance(dictionary, NativeDictionary):
            raise TypeError("the native line walker needs the native dictionary")
        import _megaforth_native as extension

        evaluator = runtime._require_bios_evaluator()
        bank0 = runtime._bank0
        interpret_primitives, defining_primitives = _checked_line_primitives(runtime)
        self._runtime = runtime
        self._evaluator = evaluator
        self._extension = extension
        # Record lines Python ran because the evaluator's state barred the
        # walker, which never saw them.
        self.barred = 0
        self._native = extension.NativeLineWalker(
            dictionary.native,
            {
                kind.name: kind.value
                for kind in DirectiveKind
                if kind is not DirectiveKind.NONAME
            },
            [
                (word.xt, word.name.decode("ascii"))
                for word in (*interpret_primitives, *defining_primitives)
            ],
            extension.WALK_RECORDS,
            # A record line writes one walker cell: EVAL-LINE.
            (0, 0, 0, 0, 0, evaluator.line_address),
            (
                evaluator.status_address,
                evaluator.column_address,
                evaluator.depth_address,
                evaluator.throw_address,
            ),
            0,
            0,
            runtime._bootstrap_numeric_base,
            bank0.base,
            bank0.limit,
        )

    def open(self, records: bytes, table: int, slots: int) -> _NativeSlots:
        """Begin one stream: the words its slots will be bound to."""

        return _NativeSlots(
            self._native,
            self._extension.NativeTokenStream(records, table, slots),
            self._runtime.dictionary,
        )

    def replay(self, stream, state, empty) -> None:
        """Evaluate STREAM's record lines natively until one needs Python.

        Afterwards the record at ``stream.index`` is the replay loop's to
        take: a line the walker rejects, END, or a record it cannot decode.
        """

        runtime = self._runtime
        context = state.context
        meter = state.meter
        compiler_state = self._evaluator.compiler_state
        if state.cursor is not empty or not _state_admits(
            runtime, context, meter, compiler_state
        ):
            self.barred += 1
            return
        compiler = compiler_state.compiler
        data = context.data
        allowance = -1 if meter.budget is None else meter.budget - meter.steps
        try:
            result = self._native.replay(
                stream.words.native,
                stream.index,
                stream.line,
                (data._floor, data._empty_pointer, data._pointer),
                None if compiler is None else _export_compiler(compiler),
                allowance,
                runtime._dictionary_base,
                runtime._dictionary_limit,
            )
        except BaseException:
            settled = self._native.take_error_state()
            if settled is not None:
                self._settle(settled, stream, context, meter)
            raise
        self._settle(result, stream, context, meter)

    def stats(self) -> tuple[int, int, int, dict[str, int]]:
        """Record lines evaluated natively, record lines the walker handed
        to Python, lines it was barred from, and, while reasons are
        collected, why each line was handed back."""

        native, python, reasons = self._native.record_stats()
        return native, python, self.barred, dict(reasons)

    def collect_reasons(self, collect: bool = True) -> None:
        self._native.collect_reasons(collect)

    def _settle(self, result, stream, context, meter) -> None:
        index, line, data_pointer, steps, compiler = result
        stream.index = index
        stream.line = line
        context.data._pointer = data_pointer
        if steps:
            # Batched exactly as after a native execution interval.
            meter.steps += steps
            self._runtime.diagnostics.account_work_many(steps)
            self._runtime.timer.advance_by(steps)
        self._evaluator.compiler_state.compiler = _import_compiler(
            compiler, "<TOKEN-REPLAY>"
        )


def _export_compiler(compiler: _Compiler) -> tuple:
    controls = []
    for frame in compiler.controls:
        if isinstance(frame, _IfFrame):
            controls.append((_FRAME_IF, frame.branch_index, 0, frame.has_else, -1, [], 0))
        elif isinstance(frame, _DoFrame):
            controls.append((
                _FRAME_DO, frame.body_target, 0, False,
                -1 if frame.question_index is None else frame.question_index,
                list(frame.leave_indices), 0,
            ))
        elif isinstance(frame, _BeginFrame):
            controls.append((_FRAME_BEGIN, frame.body_target, 0, False, -1, [], 0))
        elif isinstance(frame, _WhileFrame):
            controls.append((
                _FRAME_WHILE, frame.body_target, frame.branch_index, False, -1, [], 0,
            ))
        elif isinstance(frame, _CaseFrame):
            controls.append((_FRAME_CASE, 0, 0, False, -1, list(frame.end_indices), 0))
        else:
            case_ref = next(
                index for index, candidate in enumerate(compiler.controls)
                if candidate is frame.case
            )
            controls.append((_FRAME_OF, frame.false_index, 0, False, -1, [], case_ref))
    return (
        compiler.name,
        compiler.location,
        compiler.compile_mode,
        compiler.temporary,
        [operation_to_native(operation) for operation in compiler.operations],
        controls,
        bytes(compiler.literal_pool),
    )


def _import_compiler(
    state: tuple | None,
    source_name: str = "<EVALUATE>",
) -> _Compiler | None:
    if state is None:
        return None
    name, location, column, compile_mode, temporary, operations, controls, pool = state
    if temporary:
        raise AssertionError("the native walker never holds a temporary compiler")
    if location is None:
        # A definition the walker opened: its name token's place in the
        # input, which is always one line, the BIOS EVALUATE input
        # ("<EVALUATE>") or a TEXT record ("<TOKEN-REPLAY>").
        location = SourceLocation(source_name, column, 1, column)
    frames: list = []
    for kind, a, b, flag, question, items, case_ref in controls:
        if kind == _FRAME_IF:
            frames.append(_IfFrame(a, flag))
        elif kind == _FRAME_DO:
            frames.append(_DoFrame(a, None if question < 0 else question, list(items)))
        elif kind == _FRAME_BEGIN:
            frames.append(_BeginFrame(a))
        elif kind == _FRAME_WHILE:
            frames.append(_WhileFrame(a, b))
        elif kind == _FRAME_CASE:
            frames.append(_CaseFrame(list(items)))
        else:
            frames.append(_OfFrame(frames[case_ref], a))
    return _Compiler(
        name,
        location,
        compile_mode,
        [operation_from_native(*operation) for operation in operations],
        frames,
        bytearray(pool),
    )


__all__ = ["NativeSourceWalker", "NativeTokenWalker"]
