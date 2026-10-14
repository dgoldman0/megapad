"""Drive a KDOS loader overlay's line loop through the native line walker.

``_megaforth_native.NativeLineWalker`` runs the loop of the
``SOURCE-EVALUATE-CHECKED`` or ``_LD-WALK`` overlay.  It evaluates each line
the checked-line rule admits, with every guest-visible effect of the Python
fast path, and stops at a line it does not admit so the overlay can evaluate
that line on its ordinary path.  See ``docs/simulator-native-evaluator.md``.

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
        self._native = extension.NativeLineWalker(
            native,
            {kind.name: kind.value for kind in DirectiveKind},
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
        compiler = compiler_state.compiler
        overlays = runtime._colon_accelerators
        checked = overlays.get(self._evaluate_checked.xt)
        if checked is None or checked.word is not self._evaluate_checked:
            return False
        if self._status_throw is not None:
            translator = overlays.get(self._status_throw.xt)
            if translator is None or translator.word is not self._status_throw:
                return False
        return (
            compiler_state.conditional_skip is None
            and (compiler is None or not compiler.temporary)
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

    def _settle(self, result, context, meter, blocked: bool) -> Any:
        (
            outcome, current, evaluated, data_pointer, return_pointer, cookie,
            steps, compiler, cleared_token,
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
        if outcome == 0:
            return WALK_DONE
        if outcome == 2:
            return WALK_ITERATION
        return current, evaluated


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


def _import_compiler(state: tuple | None) -> _Compiler | None:
    if state is None:
        return None
    name, location, column, compile_mode, temporary, operations, controls, pool = state
    if temporary:
        raise AssertionError("the native walker never holds a temporary compiler")
    if location is None:
        # A definition the walker opened: its name token's place in the
        # BIOS EVALUATE input, which is always one-line "<EVALUATE>" source.
        location = SourceLocation("<EVALUATE>", column, 1, column)
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


__all__ = ["NativeSourceWalker"]
