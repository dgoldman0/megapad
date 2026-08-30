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


class StepBudgetExceeded(ExecutionError):
    """Execution consumed a caller-provided semantic step budget."""

    def __init__(self, budget: int) -> None:
        self.budget = budget
        super().__init__(f"semantic execution exceeded its {budget}-step budget")


__all__ = [
    "ExecutionError",
    "SimulatorError",
    "SourceError",
    "StepBudgetExceeded",
]
