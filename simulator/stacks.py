"""Ordered MegaForth data and return stacks.

The hosted runtime keeps return continuations, user ``>R`` values, and loop
parameters on one stack.  Keeping that ordering explicit is important: a
continuation is not a marker that words such as ``I`` may silently search
past.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, TypeAlias

from shared.cells import u64


class StackError(RuntimeError):
    """Base class for simulator stack failures."""


class StackUnderflow(StackError):
    """An operation required more stack entries than were present."""

    def __init__(
        self,
        stack: str,
        operation: str,
        *,
        required: int,
        available: int,
    ) -> None:
        self.stack = stack
        self.operation = operation
        self.required = required
        self.available = available
        noun = "entry" if required == 1 else "entries"
        super().__init__(
            f"{stack} stack underflow during {operation}: "
            f"requires {required} {noun}, has {available}"
        )


class ReturnStackShapeError(StackError):
    """The ordered return stack does not have the required entry kinds."""

    def __init__(self, operation: str, expected: str, actual: str) -> None:
        self.operation = operation
        self.expected = expected
        self.actual = actual
        super().__init__(
            f"return stack shape error during {operation}: "
            f"expected {expected}, found {actual}"
        )


@dataclass(frozen=True, slots=True)
class Continuation:
    """Internal colon-definition continuation stored on the return stack."""

    xt: int
    ip: int
    root: bool = False

    def __post_init__(self) -> None:
        object.__setattr__(self, "xt", u64(self.xt))
        object.__setattr__(self, "ip", u64(self.ip))


ReturnEntry: TypeAlias = int | Continuation


class DataStack:
    """A LIFO stack of unsigned 64-bit guest cells."""

    def __init__(self, cells: Iterable[int] = ()) -> None:
        self._cells = [u64(cell) for cell in cells]

    def push(self, cell: int) -> None:
        self._cells.append(u64(cell))

    def pop(self) -> int:
        self._require(1, "pop")
        return self._cells.pop()

    def peek(self, offset: int = 0) -> int:
        """Return the cell *offset* entries below the top without removing it."""

        if offset < 0:
            raise ValueError("data stack peek offset must be nonnegative")
        self._require(offset + 1, "peek")
        return self._cells[-1 - offset]

    def depth(self) -> int:
        return len(self._cells)

    def snapshot(self) -> tuple[int, ...]:
        """Return an immutable bottom-to-top view of the stack."""

        return tuple(self._cells)

    def _require(self, required: int, operation: str) -> None:
        available = len(self._cells)
        if available < required:
            raise StackUnderflow(
                "data",
                operation,
                required=required,
                available=available,
            )


class ReturnStack:
    """The single ordered stack for user cells, loops, and continuations."""

    def __init__(self) -> None:
        self._entries: list[ReturnEntry] = []

    def push(self, cell: int) -> None:
        """Implement the stack mutation performed by user ``>R``."""

        self._entries.append(u64(cell))

    def pop(self) -> int:
        """Implement user ``R>``, rejecting an exposed continuation."""

        entry = self._peek_entry(0, "R>")
        if isinstance(entry, Continuation):
            raise self._shape_error("R>", "user cell", entry)
        self._entries.pop()
        return entry

    def peek(self) -> int:
        """Implement user ``R@``, rejecting an exposed continuation."""

        entry = self._peek_entry(0, "R@")
        if isinstance(entry, Continuation):
            raise self._shape_error("R@", "user cell", entry)
        return entry

    def push_continuation(
        self,
        xt: int,
        ip: int,
        *,
        root: bool = False,
    ) -> Continuation:
        continuation = Continuation(xt=xt, ip=ip, root=root)
        self._entries.append(continuation)
        return continuation

    def pop_continuation(self) -> Continuation:
        entry = self._peek_entry(0, "return")
        if not isinstance(entry, Continuation):
            raise self._shape_error("return", "continuation", entry)
        self._entries.pop()
        return entry

    def enter_do(self, limit: int, index: int) -> None:
        """Place one ``DO`` loop frame as limit followed by index."""

        self._entries.append(u64(limit))
        self._entries.append(u64(index))

    def loop(self) -> bool:
        """Advance ``LOOP`` and return whether its body should repeat.

        The increment is modulo 2**64.  Reaching the limit terminates the
        loop and removes exactly its index and limit; otherwise only the
        index entry is replaced.
        """

        limit, index = self._loop_frame("LOOP", offset=0)
        next_index = u64(index + 1)
        if next_index == limit:
            del self._entries[-2:]
            return False
        self._entries[-1] = next_index
        return True

    def i(self) -> int:
        """Return the innermost loop index at its exact stack position."""

        _, index = self._loop_frame("I", offset=0)
        return index

    def j(self) -> int:
        """Return the next-outer loop index at its exact stack position."""

        self._require(4, "J")
        self._loop_frame("J", offset=0)
        _, index = self._loop_frame("J", offset=2)
        return index

    def unloop(self) -> None:
        """Remove exactly the current loop's index and limit."""

        self._loop_frame("UNLOOP", offset=0)
        del self._entries[-2:]

    def depth(self) -> int:
        return len(self._entries)

    def snapshot(self) -> tuple[ReturnEntry, ...]:
        """Return an immutable bottom-to-top view of the ordered stack."""

        return tuple(self._entries)

    def _peek_entry(self, offset: int, operation: str) -> ReturnEntry:
        self._require(offset + 1, operation)
        return self._entries[-1 - offset]

    def _loop_frame(self, operation: str, *, offset: int) -> tuple[int, int]:
        """Read one fixed-position limit/index pair without searching."""

        self._require(offset + 2, operation)
        index_entry = self._entries[-1 - offset]
        limit_entry = self._entries[-2 - offset]
        if isinstance(index_entry, Continuation):
            raise self._shape_error(
                operation,
                f"loop index cell at offset {offset}",
                index_entry,
            )
        if isinstance(limit_entry, Continuation):
            raise self._shape_error(
                operation,
                f"loop limit cell at offset {offset + 1}",
                limit_entry,
            )
        return limit_entry, index_entry

    def _require(self, required: int, operation: str) -> None:
        available = len(self._entries)
        if available < required:
            raise StackUnderflow(
                "return",
                operation,
                required=required,
                available=available,
            )

    @staticmethod
    def _shape_error(
        operation: str,
        expected: str,
        actual: ReturnEntry,
    ) -> ReturnStackShapeError:
        actual_kind = (
            "continuation" if isinstance(actual, Continuation) else "user cell"
        )
        return ReturnStackShapeError(operation, expected, actual_kind)


__all__ = [
    "Continuation",
    "DataStack",
    "ReturnEntry",
    "ReturnStack",
    "ReturnStackShapeError",
    "StackError",
    "StackUnderflow",
]
