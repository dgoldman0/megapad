"""Ordered MegaForth data and return stacks.

The hosted runtime keeps return continuations, user ``>R`` values, and loop
parameters on one stack.  Keeping that ordering explicit is important: a
continuation is not a marker that words such as ``I`` may silently search
past.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, TypeAlias

from shared.cells import CELL_BYTES, MASK64, u64
from simulator.memory import SparseAddressSpace


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


class StackOverflow(StackError):
    """A downward-growing backed stack has no free cell remaining."""

    def __init__(self, stack: str, *, floor: int) -> None:
        self.stack = stack
        self.floor = floor
        super().__init__(
            f"{stack} stack overflow: no cell remains above "
            f"0x{floor:016x}"
        )


class StackPointerError(StackError):
    """A guest stack-pointer restore names an invalid stack boundary."""

    def __init__(
        self,
        stack: str,
        pointer: object,
        *,
        floor: int,
        empty_pointer: int,
        reason: str,
    ) -> None:
        self.stack = stack
        self.pointer = pointer
        self.floor = floor
        self.empty_pointer = empty_pointer
        self.reason = reason
        super().__init__(
            f"invalid {stack} stack pointer {pointer!r}: {reason}; expected an "
            f"8-byte boundary in [0x{floor:016x}, "
            f"0x{empty_pointer:016x}]"
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
    dispatch_id: int = 0
    fault_abort: bool = False

    def __post_init__(self) -> None:
        object.__setattr__(self, "xt", u64(self.xt))
        object.__setattr__(self, "ip", u64(self.ip))
        object.__setattr__(self, "dispatch_id", u64(self.dispatch_id))
        if self.dispatch_id and not self.root:
            raise ValueError("only a root continuation may name a dispatch")
        if self.root and self.fault_abort:
            raise ValueError("a continuation cannot be both root and fault-abort")


ReturnEntry: TypeAlias = int | Continuation


def _backing_bounds(
    memory: SparseAddressSpace | None,
    floor: int | None,
    empty_pointer: int | None,
    *,
    stack: str,
) -> tuple[int, int] | None:
    """Validate one optional caller-owned, downward-growing stack span."""

    if memory is None:
        if floor is not None or empty_pointer is not None:
            raise ValueError(
                f"unbacked {stack} stack cannot specify guest pointer bounds"
            )
        return None
    if not isinstance(memory, SparseAddressSpace):
        raise TypeError("stack memory must be a SparseAddressSpace or None")
    if floor is None or empty_pointer is None:
        raise ValueError(
            f"backed {stack} stack requires floor and empty_pointer"
        )
    if not isinstance(floor, int) or not isinstance(empty_pointer, int):
        raise TypeError("stack pointer bounds must be integers")
    if not 0 <= floor <= MASK64 or not 0 <= empty_pointer <= MASK64:
        raise ValueError("stack pointer bounds must be uint64 addresses")
    if floor >= empty_pointer:
        raise ValueError("stack floor must be below its empty pointer")
    if floor % CELL_BYTES or empty_pointer % CELL_BYTES:
        raise ValueError("stack pointer bounds must be 8-byte aligned")
    containing_region = next(
        (
            region
            for region in memory.regions
            if region.base <= floor and empty_pointer <= region.limit
        ),
        None,
    )
    if containing_region is None:
        raise ValueError("complete stack bounds must lie in one memory region")
    return floor, empty_pointer


class DataStack:
    """A LIFO stack of unsigned 64-bit guest cells."""

    def __init__(
        self,
        cells: Iterable[int] = (),
        *,
        memory: SparseAddressSpace | None = None,
        floor: int | None = None,
        empty_pointer: int | None = None,
    ) -> None:
        bounds = _backing_bounds(
            memory,
            floor,
            empty_pointer,
            stack="data",
        )
        initial_cells = tuple(u64(cell) for cell in cells)
        self._memory = memory
        self._cells: list[int] | None
        self._floor: int | None
        self._empty_pointer: int | None
        self._pointer: int | None
        if bounds is None:
            self._cells = list(initial_cells)
            self._floor = None
            self._empty_pointer = None
            self._pointer = None
            return

        self._cells = None
        self._floor, self._empty_pointer = bounds
        self._pointer = self._empty_pointer
        if len(initial_cells) > self.capacity:
            raise StackOverflow("data", floor=self._floor)
        for cell in initial_cells:
            self.push(cell)

    @property
    def backed(self) -> bool:
        """Whether cells occupy the caller's shared guest address space."""

        return self._memory is not None

    @property
    def pointer(self) -> int:
        """Return the guest address of TOS, or the empty-stack boundary."""

        self._require_backing("read its pointer")
        assert self._pointer is not None
        return self._pointer

    @property
    def floor(self) -> int:
        """Return the lowest valid pointer in this caller-owned stack span."""

        self._require_backing("read its floor")
        assert self._floor is not None
        return self._floor

    @property
    def empty_pointer(self) -> int:
        """Return the pointer value representing an empty backed stack."""

        self._require_backing("read its empty pointer")
        assert self._empty_pointer is not None
        return self._empty_pointer

    @property
    def capacity(self) -> int:
        """Return the bounded cell capacity of a backed stack."""

        self._require_backing("read its capacity")
        assert self._floor is not None
        assert self._empty_pointer is not None
        return (self._empty_pointer - self._floor) // CELL_BYTES

    def push(self, cell: int) -> None:
        value = u64(cell)
        if self._memory is None:
            assert self._cells is not None
            self._cells.append(value)
            return
        target = self._push_address()
        self._memory.write64(target, value)
        self._pointer = target

    def pop(self) -> int:
        self._require(1, "pop")
        if self._memory is None:
            assert self._cells is not None
            return self._cells.pop()
        assert self._pointer is not None
        value = self._memory.read64(self._pointer)
        self._pointer += CELL_BYTES
        return value

    def peek(self, offset: int = 0) -> int:
        """Return the cell *offset* entries below the top without removing it."""

        if offset < 0:
            raise ValueError("data stack peek offset must be nonnegative")
        self._require(offset + 1, "peek")
        if self._memory is None:
            assert self._cells is not None
            return self._cells[-1 - offset]
        assert self._pointer is not None
        assert self._memory is not None
        return self._memory.read64(self._pointer + offset * CELL_BYTES)

    def depth(self) -> int:
        if self._memory is None:
            assert self._cells is not None
            return len(self._cells)
        assert self._pointer is not None
        assert self._empty_pointer is not None
        return (self._empty_pointer - self._pointer) // CELL_BYTES

    def clear(self) -> None:
        """Discard every guest cell in the active task's data stack."""

        if self._memory is None:
            assert self._cells is not None
            self._cells.clear()
        else:
            # Machine stack pops never erase RAM.  Clearing moves only the
            # active frontier so SP! may still expose retained bytes later.
            self._pointer = self._empty_pointer

    def snapshot(self) -> tuple[int, ...]:
        """Return an immutable bottom-to-top view of the stack."""

        if self._memory is None:
            assert self._cells is not None
            return tuple(self._cells)
        return tuple(
            self.peek(offset)
            for offset in range(self.depth() - 1, -1, -1)
        )

    def set_pointer(self, pointer: int) -> None:
        """Set a backed data-stack frontier without changing retained bytes."""

        self._validate_pointer(pointer)
        self._pointer = pointer

    def restore_from_top(self) -> None:
        """Implement BIOS ``SP!``: load DSP from TOS without a normal pop."""

        self._require_backing("restore its pointer from TOS")
        self._require(1, "SP!")
        assert self._memory is not None
        assert self._pointer is not None
        target = self._memory.read64(self._pointer)
        self._validate_pointer(target)
        self._pointer = target

    def _require(self, required: int, operation: str) -> None:
        available = self.depth()
        if available < required:
            raise StackUnderflow(
                "data",
                operation,
                required=required,
                available=available,
            )

    def _push_address(self) -> int:
        assert self._pointer is not None
        assert self._floor is not None
        if self._pointer <= self._floor:
            raise StackOverflow("data", floor=self._floor)
        return self._pointer - CELL_BYTES

    def _validate_pointer(self, pointer: object) -> None:
        self._require_backing("set its pointer")
        assert self._floor is not None
        assert self._empty_pointer is not None
        reason: str | None = None
        if not isinstance(pointer, int):
            reason = "pointer is not an integer"
        elif not self._floor <= pointer <= self._empty_pointer:
            reason = "pointer is outside the caller-owned stack span"
        elif pointer % CELL_BYTES:
            reason = "pointer is not 8-byte aligned"
        if reason is not None:
            raise StackPointerError(
                "data",
                pointer,
                floor=self._floor,
                empty_pointer=self._empty_pointer,
                reason=reason,
            )

    def _require_backing(self, operation: str) -> None:
        if self._memory is None:
            raise RuntimeError(f"unbacked data stack cannot {operation}")


class ReturnStack:
    """The single ordered stack for user cells, loops, and continuations."""

    def __init__(
        self,
        *,
        memory: SparseAddressSpace | None = None,
        floor: int | None = None,
        empty_pointer: int | None = None,
    ) -> None:
        bounds = _backing_bounds(
            memory,
            floor,
            empty_pointer,
            stack="return",
        )
        self._memory = memory
        self._entries: list[ReturnEntry] | None
        self._continuations: dict[int, tuple[Continuation, int]]
        self._pointer_capture_generation = 0
        self._continuation_cookie = 0
        self._floor: int | None
        self._empty_pointer: int | None
        self._pointer: int | None
        if bounds is None:
            self._entries = []
            self._continuations = {}
            self._floor = None
            self._empty_pointer = None
            self._pointer = None
        else:
            self._entries = None
            self._continuations = {}
            self._floor, self._empty_pointer = bounds
            self._pointer = self._empty_pointer

    @property
    def backed(self) -> bool:
        """Whether entries occupy the caller's shared guest address space."""

        return self._memory is not None

    @property
    def pointer(self) -> int:
        """Return the guest address of the active top return-stack slot."""

        self._require_backing("read its pointer")
        assert self._pointer is not None
        return self._pointer

    @property
    def floor(self) -> int:
        """Return the lowest valid pointer in this caller-owned stack span."""

        self._require_backing("read its floor")
        assert self._floor is not None
        return self._floor

    @property
    def empty_pointer(self) -> int:
        """Return the pointer value representing an empty backed stack."""

        self._require_backing("read its empty pointer")
        assert self._empty_pointer is not None
        return self._empty_pointer

    @property
    def capacity(self) -> int:
        """Return the bounded cell capacity of a backed return stack."""

        self._require_backing("read its capacity")
        assert self._floor is not None
        assert self._empty_pointer is not None
        return (self._empty_pointer - self._floor) // CELL_BYTES

    def push(self, cell: int) -> None:
        """Implement the stack mutation performed by user ``>R``."""

        value = u64(cell)
        if self._memory is None:
            assert self._entries is not None
            self._entries.append(value)
            return
        target = self._push_address()
        self._memory.write64(target, value)
        self._continuations.pop(target, None)
        self._pointer = target

    def pop(self) -> int:
        """Implement user ``R>``, rejecting an exposed continuation."""

        entry = self._peek_entry(0, "R>")
        if isinstance(entry, Continuation):
            raise self._shape_error("R>", "user cell", entry)
        self._discard_entries(1)
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
        dispatch_id: int = 0,
        fault_abort: bool = False,
    ) -> Continuation:
        continuation = Continuation(
            xt=xt,
            ip=ip,
            root=root,
            dispatch_id=dispatch_id,
            fault_abort=fault_abort,
        )
        if self._memory is None:
            assert self._entries is not None
            self._entries.append(continuation)
            return continuation
        target = self._push_address()
        # The dispatcher needs richer metadata than one machine cell.  Retain
        # it by address while exposing a deterministic opaque cookie to
        # ordinary guest memory reads.  In particular, writing the semantic
        # XT into this machine-private slot must not preserve its host type.
        raw = self._next_continuation_cookie(continuation.xt)
        assert self._memory is not None
        self._memory.write64(target, raw)
        self._continuations[target] = (continuation, raw)
        self._pointer = target
        return continuation

    def pop_continuation(self) -> Continuation:
        entry = self._peek_entry(0, "return")
        if not isinstance(entry, Continuation):
            raise self._shape_error("return", "continuation", entry)
        self._discard_entries(1)
        return entry

    def has_fault_abort_continuation(self) -> bool:
        """Whether a live dictionary-fault fail-closed frame remains."""

        return any(
            isinstance(entry, Continuation) and entry.fault_abort
            for entry in self.snapshot()
        )

    def enter_do(self, limit: int, index: int) -> None:
        """Place one ``DO`` loop frame as limit followed by index."""

        self.push(limit)
        self.push(index)

    def loop(self) -> bool:
        """Advance ``LOOP`` and return whether its body should repeat.

        The increment is modulo 2**64.  Reaching the limit terminates the
        loop and removes exactly its index and limit; otherwise only the
        index entry is replaced.
        """

        limit, index = self._loop_frame("LOOP", offset=0)
        next_index = u64(index + 1)
        if next_index == limit:
            self._discard_entries(2)
            return False
        if self._memory is None:
            assert self._entries is not None
            self._entries[-1] = next_index
        else:
            assert self._pointer is not None
            assert self._memory is not None
            self._memory.write64(self._pointer, next_index)
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
        self._discard_entries(2)

    def depth(self) -> int:
        if self._memory is None:
            assert self._entries is not None
            return len(self._entries)
        assert self._pointer is not None
        assert self._empty_pointer is not None
        return (self._empty_pointer - self._pointer) // CELL_BYTES

    def clear(self) -> None:
        """Discard user cells, loop frames, and internal continuations."""

        if self._memory is None:
            assert self._entries is not None
            self._entries.clear()
        else:
            # Retain bytes and continuation metadata.  A later RP! may restore
            # a saved frontier into these exact slots.  Live RP@ registrations
            # also survive until the enclosing host guard observes ABORT and
            # restores its dispatch-scoped capture checkpoint.
            self._pointer = self._empty_pointer

    def snapshot(self) -> tuple[ReturnEntry, ...]:
        """Return an immutable bottom-to-top view of the ordered stack."""

        if self._memory is None:
            assert self._entries is not None
            return tuple(self._entries)
        return tuple(
            self._peek_entry(offset, "snapshot")
            for offset in range(self.depth() - 1, -1, -1)
        )

    def restore(self, snapshot: tuple[ReturnEntry, ...]) -> None:
        """Restore an earlier snapshot after an aborted semantic dispatch.

        The dispatcher has no resumable instruction pointer when execution
        raises.  Restoring the complete ordered stack therefore prevents a
        budget abort or primitive failure from leaving internal
        continuations and partially advanced loop frames in a reusable task
        context.
        """

        if not isinstance(snapshot, tuple):
            raise TypeError("return stack snapshot must be a tuple")
        entries: list[ReturnEntry] = []
        for entry in snapshot:
            if isinstance(entry, Continuation):
                entries.append(entry)
            elif isinstance(entry, int):
                entries.append(u64(entry))
            else:
                raise TypeError(
                    "return stack snapshot entries must be cells or continuations"
                )
        if self._memory is None:
            self._entries = entries
            return

        if len(entries) > self.capacity:
            assert self._floor is not None
            raise StackOverflow("return", floor=self._floor)
        self._pointer = self._empty_pointer
        self._pointer_capture_generation = 0
        for entry in entries:
            if isinstance(entry, Continuation):
                self.push_continuation(
                    entry.xt,
                    entry.ip,
                    root=entry.root,
                    dispatch_id=entry.dispatch_id,
                    fault_abort=entry.fault_abort,
                )
            else:
                self.push(entry)

    def set_pointer(self, pointer: int) -> None:
        """Set a backed return-stack frontier without erasing retained slots."""

        self._validate_pointer(pointer)
        self._pointer = pointer

    def capture_pointer(self) -> int:
        """Return and register a frontier observed in this host dispatch."""

        pointer = self.pointer
        self._pointer_capture_generation += 1
        return pointer

    def pointer_capture_checkpoint(self) -> int:
        """Capture ``RP@`` registrations around one host dispatch boundary."""

        return self._pointer_capture_generation

    def restore_pointer_captures(self, checkpoint: int) -> None:
        """Restore a trusted capture registry after nested/failed dispatch."""

        if not isinstance(checkpoint, int) or checkpoint < 0:
            raise TypeError(
                "return pointer capture checkpoint must be a nonnegative integer"
            )
        self._pointer_capture_generation = checkpoint

    def has_pointer_captures_after(self, checkpoint: int) -> bool:
        """Whether this dispatch observed any additional ``RP@`` frontier."""

        if not isinstance(checkpoint, int) or checkpoint < 0:
            raise TypeError(
                "return pointer capture checkpoint must be a nonnegative integer"
            )
        return self._pointer_capture_generation > checkpoint

    def _peek_entry(self, offset: int, operation: str) -> ReturnEntry:
        self._require(offset + 1, operation)
        if self._memory is None:
            assert self._entries is not None
            return self._entries[-1 - offset]
        assert self._pointer is not None
        assert self._memory is not None
        address = self._pointer + offset * CELL_BYTES
        raw = self._memory.read64(address)
        typed = self._continuations.get(address)
        if typed is None:
            return raw
        continuation, expected_raw = typed
        if raw == expected_raw:
            return continuation
        # A raw guest store replaced this return slot.  Shared memory is
        # authoritative; do not resurrect stale host-only type metadata.
        del self._continuations[address]
        return raw

    def _loop_frame(self, operation: str, *, offset: int) -> tuple[int, int]:
        """Read one fixed-position limit/index pair without searching."""

        self._require(offset + 2, operation)
        index_entry = self._peek_entry(offset, operation)
        limit_entry = self._peek_entry(offset + 1, operation)
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
        available = self.depth()
        if available < required:
            raise StackUnderflow(
                "return",
                operation,
                required=required,
                available=available,
            )

    def _discard_entries(self, count: int) -> None:
        if self._memory is None:
            assert self._entries is not None
            del self._entries[-count:]
        else:
            assert self._pointer is not None
            self._pointer += count * CELL_BYTES

    def _push_address(self) -> int:
        assert self._pointer is not None
        assert self._floor is not None
        if self._pointer <= self._floor:
            raise StackOverflow("return", floor=self._floor)
        return self._pointer - CELL_BYTES

    def _validate_pointer(self, pointer: object) -> None:
        self._require_backing("set its pointer")
        assert self._floor is not None
        assert self._empty_pointer is not None
        reason: str | None = None
        if not isinstance(pointer, int):
            reason = "pointer is not an integer"
        elif not self._floor <= pointer <= self._empty_pointer:
            reason = "pointer is outside the caller-owned stack span"
        elif pointer % CELL_BYTES:
            reason = "pointer is not 8-byte aligned"
        if reason is not None:
            raise StackPointerError(
                "return",
                pointer,
                floor=self._floor,
                empty_pointer=self._empty_pointer,
                reason=reason,
            )

    def _require_backing(self, operation: str) -> None:
        if self._memory is None:
            raise RuntimeError(f"unbacked return stack cannot {operation}")

    def _next_continuation_cookie(self, xt: int) -> int:
        while True:
            self._continuation_cookie += 1
            raw = u64(0xC07E_CAFE_0000_0000 ^ self._continuation_cookie)
            if raw != xt:
                return raw

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
    "StackOverflow",
    "StackPointerError",
    "StackUnderflow",
]
