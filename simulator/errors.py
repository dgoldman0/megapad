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


class ExecutionBlocked(SimulatorError):
    """Completion-only execution reached a resumable IDL boundary."""

    def __init__(self, suspension: object, semantic_steps: int) -> None:
        self.suspension = suspension
        self.semantic_steps = semantic_steps
        super().__init__(
            "semantic execution blocked at IDL after "
            f"{semantic_steps} step(s); deliver a wake and resume the suspension"
        )


class InstructionFault(ExecutionError):
    """A primitive reached an operation that traps on the machine.

    The runtime routes it like the BIOS instruction-fault handlers: the
    ``FAULT-XT!`` callback receives ``throw_code``, and when there is none or
    it returns, the runtime prints ``report_for(core)`` for the core that
    faulted and aborts.
    """

    title = ""
    throw_code = 0

    def report_for(self, core: int) -> bytes:
        """The BIOS's report line, naming the faulting core."""

        return f"\r\n*** {self.title} CORE={core:02X}\r\n".encode()

    @property
    def report(self) -> bytes:
        return self.report_for(0)


class IllegalInstructionFault(InstructionFault):
    """The machine raises ``IVEC_ILLEGAL_OP``."""

    title = "ILLEGAL INSTRUCTION"
    throw_code = -21  # unsupported operation


class TaskWordRefused(IllegalInstructionFault):
    """``PAUSE`` or ``TASK-YIELD`` where it cannot run: the illegal-operation
    fault, -21, that the BIOS's words raise at their reserved ILLEGAL byte
    and that its report names as their refusal."""

    title = "PAUSE OR TASK-YIELD REFUSED"


class DivideByZeroFault(InstructionFault):
    """The machine raises ``IVEC_DIV_ZERO`` (zero divisor or overflow)."""

    title = "DIVIDE BY ZERO"
    throw_code = -10  # division by zero


class AlignmentFault(InstructionFault):
    """The machine raises ``IVEC_ALIGN_FAULT`` at ``address``.

    The TACC words raise it for an image or source row that is not 64-byte
    aligned; its report names the address, as the BIOS's does.
    """

    throw_code = -23  # address alignment exception

    def __init__(self, address: int, message: str) -> None:
        self.address = address
        self.title = f"ALIGNMENT FAULT @ {address:016X}"
        super().__init__(message)


class ForthAbort(ExecutionError):
    """The nonreturning BIOS ``ABORT`` word cleared one active task."""

    def __init__(
        self,
        message: str,
        *,
        origin_context: object | None = None,
        quit_callback_ran: bool = False,
        keeps_data_stack: bool = False,
    ) -> None:
        self.origin_context = origin_context
        # Whether the callback installed with QUIT-XT! ran once the task's
        # stacks were clear, as BIOS QUIT runs it.
        self.quit_callback_ran = quit_callback_ran
        # QUIT keeps the data stack; ABORT, ABORT" and a fault empty it.
        self.keeps_data_stack = keeps_data_stack
        super().__init__(message)

    def bind_origin(self, context: object) -> bool:
        """Bind an untagged host primitive abort to its innermost task."""

        if self.origin_context is None:
            self.origin_context = context
        return self.origin_context is context

    def reset_origin(self, context) -> bool:
        """Bind the abort as ``bind_origin`` does and, when CONTEXT is its
        origin, give it the BIOS word's reset: the return stack cleared, and
        the data stack too unless the abort is QUIT's. Whether CONTEXT was
        the origin."""

        if not self.bind_origin(context):
            return False
        if not self.keeps_data_stack:
            context.data.clear()
        context.returns.clear()
        return True


class StepBudgetExceeded(ExecutionError):
    """Execution consumed a caller-provided semantic step budget."""

    def __init__(self, budget: int) -> None:
        self.budget = budget
        super().__init__(f"semantic execution exceeded its {budget}-step budget")


__all__ = [
    "AlignmentFault",
    "DivideByZeroFault",
    "ExecutionError",
    "ExecutionBlocked",
    "ForthAbort",
    "IllegalInstructionFault",
    "InstructionFault",
    "SimulatorError",
    "SourceError",
    "StepBudgetExceeded",
    "TaskWordRefused",
]
