"""Public failure types for hosted source evaluation and execution."""

from __future__ import annotations

from simulator.source import SourceLocation


class SimulatorError(RuntimeError):
    """Base class for hosted simulator failures."""


class SourceError(SimulatorError):
    """A source token could not be interpreted or compiled."""

    def __init__(self, message: str, location: SourceLocation) -> None:
        self.message = message
        self.location = location
        super().__init__(
            f"{location.source_name}:{location.line}:{location.column + 1}: "
            f"{message}"
        )


class ExecutionError(SimulatorError):
    """A semantic definition could not execute coherently."""


class ForthAbort(ExecutionError):
    """The nonreturning BIOS ``ABORT`` word cleared one active task."""

    def __init__(
        self,
        message: str,
        *,
        origin_context: object | None = None,
    ) -> None:
        self.origin_context = origin_context
        super().__init__(message)

    def bind_origin(self, context: object) -> bool:
        """Bind an untagged host primitive abort to its innermost task."""

        if self.origin_context is None:
            self.origin_context = context
        return self.origin_context is context


class StepBudgetExceeded(ExecutionError):
    """Execution consumed a caller-provided semantic step budget."""

    def __init__(self, budget: int) -> None:
        self.budget = budget
        super().__init__(f"semantic execution exceeded its {budget}-step budget")


__all__ = [
    "ExecutionError",
    "ForthAbort",
    "SimulatorError",
    "SourceError",
    "StepBudgetExceeded",
]
