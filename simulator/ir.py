"""Small, explicit semantic instruction set for hosted Forth definitions."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TypeAlias

from shared.cells import MASK64, u64


def _target(value: int) -> int:
    if not isinstance(value, int):
        raise TypeError("branch target must be an integer")
    if value < 0:
        raise ValueError("branch target must not be negative")
    return value


def _xt(value: int) -> int:
    if not isinstance(value, int):
        raise TypeError("execution token must be an integer")
    if not 0 < value <= MASK64:
        raise ValueError("execution token must be a nonzero uint64 value")
    return value


def _payload(value: bytes, *, operation: str) -> bytes:
    if not isinstance(value, bytes):
        raise TypeError(f"{operation} payload must be bytes")
    return value


@dataclass(frozen=True, slots=True)
class Literal:
    value: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "value", u64(self.value))


@dataclass(frozen=True, slots=True)
class Call:
    xt: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "xt", _xt(self.xt))


@dataclass(frozen=True, slots=True)
class CallSelf:
    """Call the colon definition which owns this operation."""


@dataclass(frozen=True, slots=True)
class Branch:
    target: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "target", _target(self.target))


@dataclass(frozen=True, slots=True)
class BranchZero:
    target: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "target", _target(self.target))


@dataclass(frozen=True, slots=True)
class Return:
    pass


@dataclass(frozen=True, slots=True)
class Idle:
    """Wait at one semantic host-service boundary before continuing."""


@dataclass(frozen=True, slots=True)
class UartReadAttempt:
    """Publish one queued UART byte and TRUE, or publish only FALSE."""


@dataclass(frozen=True, slots=True)
class RPush:
    pass


@dataclass(frozen=True, slots=True)
class RPop:
    pass


@dataclass(frozen=True, slots=True)
class RPeek:
    pass


@dataclass(frozen=True, slots=True)
class RPushPair:
    """Move one ordered data-stack pair onto the return stack."""


@dataclass(frozen=True, slots=True)
class RPopPair:
    """Move one ordered return-stack pair onto the data stack."""


@dataclass(frozen=True, slots=True)
class RPeekPair:
    """Copy one ordered return-stack pair onto the data stack."""


@dataclass(frozen=True, slots=True)
class PushStringLiteral:
    """Push one address/length pair from the current colon's body pool."""

    offset: int
    length: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "offset", _target(self.offset))
        object.__setattr__(self, "length", _target(self.length))


@dataclass(frozen=True, slots=True)
class Do:
    pass


@dataclass(frozen=True, slots=True)
class QuestionDo:
    target: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "target", _target(self.target))


@dataclass(frozen=True, slots=True)
class Loop:
    target: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "target", _target(self.target))


@dataclass(frozen=True, slots=True)
class Unloop:
    pass


@dataclass(frozen=True, slots=True)
class InstallDoes:
    """Attach a suffix entry point to the newest CREATE-family word."""

    entry_ip: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "entry_ip", _target(self.entry_ip))


@dataclass(frozen=True, slots=True)
class RestoreDataStackPointer:
    """Install the data-stack pointer stored in the current top cell."""


@dataclass(frozen=True, slots=True)
class RestoreReturnStackPointer:
    """Consume and install one return-stack pointer."""


@dataclass(frozen=True, slots=True)
class WriteOutput:
    """Append one compile-time literal to the BIOS output stream."""

    payload: bytes

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "payload",
            _payload(self.payload, operation="output"),
        )


@dataclass(frozen=True, slots=True)
class AbortIf:
    """Consume a flag and perform BIOS ABORT after writing a literal."""

    payload: bytes

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "payload",
            _payload(self.payload, operation='ABORT"'),
        )


Operation: TypeAlias = (
    Literal
    | Call
    | CallSelf
    | Branch
    | BranchZero
    | Return
    | Idle
    | UartReadAttempt
    | RPush
    | RPop
    | RPeek
    | RPushPair
    | RPopPair
    | RPeekPair
    | PushStringLiteral
    | Do
    | QuestionDo
    | Loop
    | Unloop
    | InstallDoes
    | RestoreDataStackPointer
    | RestoreReturnStackPointer
    | WriteOutput
    | AbortIf
)


__all__ = [
    "AbortIf",
    "Branch",
    "BranchZero",
    "Call",
    "CallSelf",
    "Do",
    "Idle",
    "InstallDoes",
    "Literal",
    "Loop",
    "Operation",
    "PushStringLiteral",
    "QuestionDo",
    "RestoreDataStackPointer",
    "RestoreReturnStackPointer",
    "Return",
    "RPeek",
    "RPeekPair",
    "RPop",
    "RPopPair",
    "RPush",
    "RPushPair",
    "Unloop",
    "UartReadAttempt",
    "WriteOutput",
]
