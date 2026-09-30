"""Engine-owned task markers which do not import the semantic dispatcher."""

from dataclasses import dataclass, field

from shared.cells import MASK64
from simulator.dictionary import Word


@dataclass(frozen=True, slots=True, eq=False)
class ForeignDefinition:
    """A marker; only the issuing engine binds it to an executable Word."""

    _registration: object = field(repr=False)


@dataclass(frozen=True, slots=True, eq=False)
class ForeignResumeTarget:
    """An original semantic location, or completion of its public root."""

    word: Word | None = field(repr=False)
    ip: int

    def __post_init__(self):
        if type(self.ip) is not int:
            raise TypeError("semantic resume IP must be an exact integer")
        if not 0 <= self.ip <= MASK64:
            raise ValueError("semantic resume IP must be in uint64 range")
        if self.word is None:
            if self.ip:
                raise ValueError("root completion must have zero semantic IP")
        elif type(self.word) is not Word:
            raise TypeError("semantic resume target must retain an exact Word")


@dataclass(frozen=True, slots=True)
class ForeignDispatchReport:
    root_id: int
    machine_instructions: int
    machine_cycles: int
    callbacks: int
    entries: int
    semantic_steps: int
    completed: bool
    cancelled: bool


@dataclass(frozen=True, slots=True, eq=False)
class ForeignCallbackTarget:
    word: Word = field(repr=False)


@dataclass(frozen=True, slots=True, eq=False)
class TaskSemanticReceiptV1:
    """Cumulative task callback work; only the engine can issue its identity."""

    root_token: object = field(repr=False)
    root_id: int
    sequence: int
    semantic_steps: int

    def __post_init__(self):
        for value, label, minimum in ((self.root_id, "root ID", 1),
                                      (self.sequence, "sequence", 1),
                                      (self.semantic_steps, "semantic steps", 0)):
            if type(value) is not int:
                raise TypeError(f"task semantic receipt {label} must be an exact integer")
            if not minimum <= value <= MASK64:
                raise ValueError(f"task semantic receipt {label} is outside uint64 range")


__all__ = ["ForeignDefinition", "ForeignResumeTarget", "ForeignDispatchReport", "TaskSemanticReceiptV1"]
