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
class RPush:
    pass


@dataclass(frozen=True, slots=True)
class RPop:
    pass


@dataclass(frozen=True, slots=True)
class RPeek:
    pass


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


Operation: TypeAlias = (
    Literal
    | Call
    | Branch
    | BranchZero
    | Return
    | RPush
    | RPop
    | RPeek
    | Do
    | QuestionDo
    | Loop
    | Unloop
)


__all__ = [
    "Branch",
    "BranchZero",
    "Call",
    "Do",
    "Literal",
    "Loop",
    "Operation",
    "QuestionDo",
    "Return",
    "RPeek",
    "RPop",
    "RPush",
    "Unloop",
]
